//===---                         OpenMP Loop Check              -----------===//
// Under the Apache License v2.0
//===----------------------------------------------------------------------===//
///
///  \file This file implements a omp-loop-check tool.
///
///  This tool uses the Clang Tooling infrastructure, see
///    http://clang.llvm.org/docs/HowToSetupToolingForLLVM.html
///  for details on setting it up with LLVM source tree.
///
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/TextDiagnostic.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include <fstream>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

// Output formats
enum class OutputFormat { CLI, MARKDOWN, HTML };

// Refactoring details
static StringRef RefactoringCode = "HACO001";
static StringRef RefactoringDescription = "Loop is not in canonical form";
static StringRef RefactoringRationale =
    "In order to parallelize a loop with OpenMP, the loop has to be in "
    "canonical form. Among the conditions of the canonical form we have that "
    "the loop has to have an init, a condition and an increment section. "
    "Additionally, the loop body cannot contain statements that terminate the "
    "loop flow.";

// AST Matchers

// Match a loop inside a function
StatementMatcher LoopMatcher =
    forStmt(hasAncestor(functionDecl().bind("function"))).bind("forLoop");

// Match a break inside a loop inside a function
StatementMatcher BreakMatcher =
    breakStmt(hasAncestor(functionDecl().bind("function")),
              hasAncestor(forStmt().bind("forLoop")))
        .bind("break");

// Get the presumed location, which includes file and line number
static std::optional<PresumedLoc> getPresumedLocation(SourceLocation Loc,
                                                      SourceManager &Source) {
  // Reject on invalid location
  PresumedLoc PresumedLocation = Source.getPresumedLoc(Loc);
  if (PresumedLocation.isInvalid()) {
    return std::nullopt;
  }

  return PresumedLocation;
}

static const VarDecl *matchVarDeclFromDeclRefExpr(const Expr *Expression) {
  // Reject on invalid expression
  if (!Expression) {
    return nullptr;
  }

  // Reject on different kind of expression
  const DeclRefExpr *CandidateExpression =
      llvm::dyn_cast_or_null<DeclRefExpr>(Expression->IgnoreParenImpCasts());
  if (!CandidateExpression) {
    return nullptr;
  }

  // Reject on different kind of declaration
  const VarDecl *CandidateVar =
      llvm::dyn_cast_or_null<VarDecl>(CandidateExpression->getDecl());
  if (!CandidateVar) {
    return nullptr;
  }

  // Reject on non integer types
  if (!CandidateVar->getType()->isIntegerType()) {
    return nullptr;
  }

  // Success: the variable was found!
  return CandidateVar;
}

static const VarDecl *matchLHSVarDeclFromOperator(const Expr *Expression) {
  // Reject on invalid expression
  if (!Expression) {
    return nullptr;
  }

  Expression = Expression->IgnoreParenImpCasts();

  // Analyze the LHS of a binary operator
  if (const BinaryOperator *BO =
          llvm::dyn_cast_or_null<BinaryOperator>(Expression)) {
    return matchVarDeclFromDeclRefExpr(BO->getLHS());
  }

  // Analyze the sub-expression of a unary operator
  if (const UnaryOperator *UO =
          llvm::dyn_cast_or_null<UnaryOperator>(Expression)) {
    return matchVarDeclFromDeclRefExpr(UO->getSubExpr());
  }

  // TODO: other operator types?

  return nullptr;
}

static const BinaryOperator *getExprAsBinaryOperator(const Expr *Expression) {
  // Reject on invalid expression
  if (!Expression) {
    return nullptr;
  }

  return llvm::dyn_cast_or_null<BinaryOperator>(
      Expression->IgnoreParenImpCasts());
}

static void emitInitSuggestionDiagnostic(raw_ostream &Out, TextDiagnostic &TD,
                                         SourceManager &Source,
                                         const VarDecl &IncVarDecl) {
  Out << "<haco_opportunity_suggestion>";

  TD.printDiagnosticMessage(
      Out, true,
      llvm::formatv(
          "Init '{0}' inside the loop header. Follow its "
          "usage from the reported declaration point to pick the right "
          "value. If the variable value does not depend on previous "
          "operations and it is not needed after the loop, "
          "better consider declaring it inside the loop header.",
          IncVarDecl.getName())
          .str(),
      0, 0, true);

  Out << "</haco_opportunity_suggestion>";

  std::optional<PresumedLoc> DeclLocation =
      getPresumedLocation(IncVarDecl.getBeginLoc(), Source);
  if (DeclLocation) {
    Out << "<haco_opportunity_extrainfo>";

    TD.emitDiagnostic(FullSourceLoc(IncVarDecl.getBeginLoc(), Source),
                      DiagnosticsEngine::Remark,
                      "declaration of loop variable here", std::nullopt,
                      std::nullopt);

    Out << "</haco_opportunity_extrainfo>";
  }
}

static void emitBreakSuggestionDiagnostic(raw_ostream &Out, TextDiagnostic &TD,
                                          SourceManager &Source,
                                          const BreakStmt &BS) {
  Out << "<haco_opportunity_suggestion>";

  TD.printDiagnosticMessage(
      Out, true,
      "Modify the logic of the loop body so it does not exit abruptly from it. "
      "The termination condition should be only handled at the loop header.",
      0, 0, true);

  Out << "</haco_opportunity_suggestion>";

  Out << "<haco_opportunity_extrainfo>";

  TD.emitDiagnostic(FullSourceLoc(BS.getBreakLoc(), Source),
                    DiagnosticsEngine::Remark, "'break' statement found here",
                    CharSourceRange::getTokenRange(
                        SourceRange(BS.getBeginLoc(), BS.getEndLoc())),
                    std::nullopt);

  Out << "</haco_opportunity_extrainfo>";
}

static void emitOpportunityInfo(raw_ostream &Out, int CurrentOpportunityNumber,
                                StringRef FunctionName) {
  Out.changeColor(llvm::raw_ostream::Colors::CYAN, true) << llvm::formatv(
      "<haco_opportunity_number>Opportunity #{0}</haco_opportunity_number> "
      "<haco_opportunity_function>at function "
      "'{1}':</haco_opportunity_function>",
      CurrentOpportunityNumber, FunctionName);

  Out.resetColor() << '\n';
}

static void emitDefect(raw_ostream &Out, TextDiagnostic &TD,
                       SourceManager &Source, SourceLocation Loc,
                       StringRef Message) {
  Out << "<haco_opportunity_defect>";

  TD.emitDiagnostic(FullSourceLoc(Loc, Source), DiagnosticsEngine::Warning,
                    Message, std::nullopt, std::nullopt);

  Out << "</haco_opportunity_defect>";
}

static void reportBreakInsideLoop(raw_ostream &Out, TextDiagnostic &TD,
                                  SourceManager &Source,
                                  int CurrentOpportunityNumber,
                                  const ForStmt &FS, const BreakStmt &BS,
                                  StringRef FunctionName) {
  Out << "<haco_opportunity_item>";

  emitOpportunityInfo(Out, CurrentOpportunityNumber, FunctionName);

  emitDefect(Out, TD, Source, FS.getForLoc(),
             "Loop body contains a 'break' statement");

  emitBreakSuggestionDiagnostic(Out, TD, Source, BS);

  Out << "</haco_opportunity_item>";
}

static void reportLoopWithoutInit(raw_ostream &Out, TextDiagnostic &TD,
                                  SourceManager &Source,
                                  int CurrentOpportunityNumber,
                                  const ForStmt &FS, StringRef FunctionName) {
  Out << "<haco_opportunity_item>";

  emitOpportunityInfo(Out, CurrentOpportunityNumber, FunctionName);

  emitDefect(Out, TD, Source, FS.getForLoc(), "Loop without init");

  // Skip on absence of increment variable
  const VarDecl *IncVarDecl = matchLHSVarDeclFromOperator(FS.getInc());
  if (!IncVarDecl) {
    Out << "</haco_opportunity_item>";
    return;
  }

  // Skip on absence of conforming condition
  const BinaryOperator *CondBO = getExprAsBinaryOperator(FS.getCond());
  if (!CondBO) {
    Out << "</haco_opportunity_item>";
    return;
  }

  // Increment and LHS condition variable are the same, suggest changes
  const VarDecl *CondLHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getLHS());
  if (CondLHSVarDecl &&
      IncVarDecl->getCanonicalDecl() == CondLHSVarDecl->getCanonicalDecl()) {
    emitInitSuggestionDiagnostic(Out, TD, Source, *IncVarDecl);
    Out << "</haco_opportunity_item>";

    return; // Skip further checks
  }

  // Increment and RHS condition variable are the same, suggest changes
  const VarDecl *CondRHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getRHS());
  if (CondRHSVarDecl &&
      IncVarDecl->getCanonicalDecl() == CondRHSVarDecl->getCanonicalDecl()) {
    emitInitSuggestionDiagnostic(Out, TD, Source, *IncVarDecl);
    Out << "</haco_opportunity_item>";

    return; // Skip further checks
  }

  // Increment and condition variable are different, make a warning
  if (CondLHSVarDecl || CondRHSVarDecl) {
    Out << "<haco_opportunity_suggestion>";
    Out << "</haco_opportunity_suggestion>";
    Out << "<haco_opportunity_extrainfo>";

    TD.emitDiagnostic(FullSourceLoc(FS.getLParenLoc(), Source),
                      DiagnosticsEngine::Remark,
                      "The condition and the increment variables are not "
                      "the same. Is this intentional?",
                      CharSourceRange::getTokenRange(
                          SourceRange(FS.getLParenLoc(), FS.getRParenLoc())),
                      std::nullopt);

    Out << "</haco_opportunity_extrainfo>";
  }

  Out << "</haco_opportunity_item>";
}

static void reportLoopWithoutCondition(raw_ostream &Out, TextDiagnostic &TD,
                                       SourceManager &Source,
                                       int CurrentOpportunityNumber,
                                       const ForStmt &FS,
                                       StringRef FunctionName) {
  Out << "<haco_opportunity_item>";
  emitOpportunityInfo(Out, CurrentOpportunityNumber, FunctionName);

  emitDefect(Out, TD, Source, FS.getForLoc(), "Loop without condition");

  // Skip on absence of increment variable
  const VarDecl *IncVarDecl = matchLHSVarDeclFromOperator(FS.getInc());
  if (!IncVarDecl) {
    Out << "</haco_opportunity_item>";
    return;
  }

  // Skip on absence of init binary operator
  const BinaryOperator *InitBO =
      llvm::dyn_cast_or_null<BinaryOperator>(FS.getInit());
  if (!InitBO) {
    Out << "</haco_opportunity_item>";

    return;
  }

  // Increment and LHS init variable are the same, suggest changes
  const VarDecl *InitVarDecl = matchVarDeclFromDeclRefExpr(InitBO->getLHS());
  if (InitVarDecl &&
      IncVarDecl->getCanonicalDecl() == InitVarDecl->getCanonicalDecl()) {
    Out << "<haco_opportunity_suggestion>";

    TD.printDiagnosticMessage(
        Out, true,
        "Init and increment variables match, but condition variable is "
        "missing.",
        0, 0, true);

    Out.resetColor() << '\n';

    Out << "</haco_opportunity_suggestion>";
    Out << "<haco_opportunity_extrainfo>";
    Out << "</haco_opportunity_extrainfo>";
    Out << "</haco_opportunity_item>";

    return; // Skip further checks
  }

  // Increment and init variable are different, make a warning
  if (InitVarDecl) {
    Out << "<haco_opportunity_suggestion>";
    Out << "</haco_opportunity_suggestion>";
    Out << "<haco_opportunity_extrainfo>";

    TD.emitDiagnostic(FullSourceLoc(FS.getLParenLoc(), Source),
                      DiagnosticsEngine::Remark,
                      "The init and the increment variables are not "
                      "the same. Is this intentional?",
                      CharSourceRange::getTokenRange(
                          SourceRange(FS.getLParenLoc(), FS.getRParenLoc())),
                      std::nullopt);

    Out.resetColor() << '\n';
    Out << "</haco_opportunity_extrainfo>";
  }
  Out << "</haco_opportunity_item>";
}

static void reportLoopWithoutIncrement(raw_ostream &Out, TextDiagnostic &TD,
                                       SourceManager &Source,
                                       int CurrentOpportunityNumber,
                                       const ForStmt &FS,
                                       StringRef FunctionName) {
  Out << "<haco_opportunity_item>";
  emitOpportunityInfo(Out, CurrentOpportunityNumber, FunctionName);

  emitDefect(Out, TD, Source, FS.getForLoc(), "Loop without increment");

  // Skip on absence of init binary operator
  const BinaryOperator *InitBO =
      llvm::dyn_cast_or_null<BinaryOperator>(FS.getInit());
  if (!InitBO) {
    Out << "</haco_opportunity_item>";

    return;
  }

  // Skip on absence of init variable
  const VarDecl *InitVarDecl = matchVarDeclFromDeclRefExpr(InitBO->getLHS());
  if (!InitVarDecl) {
    return;
  }

  // Skip on absence of conforming condition
  const BinaryOperator *CondBO = getExprAsBinaryOperator(FS.getCond());
  if (!CondBO) {
    Out << "</haco_opportunity_item>";
    return;
  }

  // Init and LHS condition variable are the same, suggest changes
  const VarDecl *CondLHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getLHS());
  if (CondLHSVarDecl &&
      InitVarDecl->getCanonicalDecl() == CondLHSVarDecl->getCanonicalDecl()) {
    TD.printDiagnosticMessage(
        Out, true,
        "Init and condition variables match, but increment variable is "
        "missing.",
        0, 0, true);

    Out.resetColor() << '\n';
    Out << "</haco_opportunity_item>";

    return; // Skip further checks
  }

  // Init and RHS condition variable are the same, suggest changes
  const VarDecl *CondRHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getRHS());
  if (CondRHSVarDecl &&
      InitVarDecl->getCanonicalDecl() == CondRHSVarDecl->getCanonicalDecl()) {
    TD.printDiagnosticMessage(
        Out, true,
        "Init and condition variables match, but increment variable is "
        "missing.",
        0, 0, true);

    Out.resetColor() << '\n';
    Out << "</haco_opportunity_item>";

    return; // Skip further checks
  }

  // Init and condition variable are different, make a warning
  if (CondLHSVarDecl || CondRHSVarDecl) {
    TD.emitDiagnostic(FullSourceLoc(FS.getLParenLoc(), Source),
                      DiagnosticsEngine::Remark,
                      "The init and the condition variables are not "
                      "the same. Is this intentional?",
                      CharSourceRange::getTokenRange(
                          SourceRange(FS.getLParenLoc(), FS.getRParenLoc())),
                      std::nullopt);

    Out.resetColor() << '\n';
  }

  Out << "</haco_opportunity_item>";
}

static bool isConformingLoop(const ForStmt *FS, SourceManager &Source) {
  // Skip non loops
  if (!FS) {
    return false;
  }

  // Skip on header files
  if (!Source.isWrittenInMainFile(FS->getForLoc())) {
    return false;
  }

  // Skip on lack of location info for loop
  if (!getPresumedLocation(FS->getForLoc(), Source)) {
    return false;
  }

  return true;
}

class LoopPrinter : public MatchFinder::MatchCallback {
private:
  int NumberOfOpportunities = 0;
  raw_ostream &Out;

public:
  LoopPrinter(raw_ostream &OutStream) : Out(OutStream) {}

  void run(const MatchFinder::MatchResult &Result) override {
    // Skip on lack of context
    ASTContext *Context = Result.Context;
    if (!Context) {
      return;
    }

    SourceManager &Source = Context->getSourceManager();

    // Skip non conforming loops
    const ForStmt *FS = Result.Nodes.getNodeAs<ForStmt>("forLoop");
    if (!isConformingLoop(FS, Source)) {
      return;
    }

    // Skip on absent containing function info
    const FunctionDecl *F = Result.Nodes.getNodeAs<FunctionDecl>("function");
    if (!F) {
      return;
    }

    StringRef FunctionName = F->getName();

    TextDiagnostic TD(Out, Context->getLangOpts(),
                      &Context->getDiagnostics().getDiagnosticOptions());

    // Report a loop with a break statement inside
    if (const BreakStmt *BS = Result.Nodes.getNodeAs<BreakStmt>("break")) {

      // Here the loops come from the AST Matcher BreakMatcher

      NumberOfOpportunities++;

      reportBreakInsideLoop(Out, TD, Source, NumberOfOpportunities, *FS, *BS,
                            FunctionName);

      return; // do not analyze further this loop
    }

    // Here the loops come from the AST Matcher LoopMatcher

    // Report a loop without init
    if (!FS->getInit()) {
      NumberOfOpportunities++;

      reportLoopWithoutInit(Out, TD, Source, NumberOfOpportunities, *FS,
                            FunctionName);
    }

    // Report loop without condition
    if (!FS->getCond()) {
      NumberOfOpportunities++;

      reportLoopWithoutCondition(Out, TD, Source, NumberOfOpportunities, *FS,
                                 FunctionName);
    }

    // Report loop without increment
    if (!FS->getInc()) {
      NumberOfOpportunities++;

      reportLoopWithoutIncrement(Out, TD, Source, NumberOfOpportunities, *FS,
                                 FunctionName);
    }
  }

  int getNumberOfOpportunities() const { return NumberOfOpportunities; }
};

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...\n");

static std::string replaceAll(const std::string &Str,    // where to work
                              const std::string &Find,   // substitute 'find'
                              const std::string &Replace //      by 'replace'
) {
  using namespace std;
  string Result;
  size_t FindLen = Find.size();
  size_t Pos, From = 0;
  while (string::npos != (Pos = Str.find(Find, From))) {
    Result.append(Str, From, Pos - From);
    Result.append(Replace);
    From = Pos + FindLen;
  }
  Result.append(Str, From, string::npos);

  return Result;
}

static void printResults(OutputFormat Format, std::string Results) {
  switch (Format) {
  case OutputFormat::HTML: {
    std::fstream HTMLFile;
    HTMLFile.open("haco_html_file.html", std::ios::out);

    if (!HTMLFile.is_open()) {
      llvm::errs() << "Error: could not open the file\n";
    }

    Results = replaceAll(Results, "<haco_header_big>", "<h1>");
    Results = replaceAll(Results, "<haco_header_medium>", "<h2>");
    Results = replaceAll(Results, "<haco_header_small>", "<h3>");
    Results = replaceAll(Results, "<haco_text>", "<p>");
    Results = replaceAll(Results, "<haco_opportunity_list>",
                         "<table border=1 "
                         "width=80%><tr><th>Number</th><th>Function</"
                         "th><th>Details</th><th>Suggestion</th><th>Hint</th>");
    Results = replaceAll(Results, "<haco_opportunity_item>", "<tr>");
    Results =
        replaceAll(Results, "<haco_opportunity_number>Opportunity #", "<td>");
    Results =
        replaceAll(Results, "<haco_opportunity_function>at function '", "<td>");
    Results = replaceAll(Results, "<haco_opportunity_defect>", "<td>");
    Results = replaceAll(Results, "<haco_opportunity_suggestion>", "<td>");
    Results = replaceAll(Results, "<haco_opportunity_extrainfo>", "<td>");
    Results = replaceAll(Results, "</haco_header_big>", "</h1>");
    Results = replaceAll(Results, "</haco_header_medium>", "</h2>");
    Results = replaceAll(Results, "</haco_header_small>", "</h3>");
    Results = replaceAll(Results, "</haco_text>", "</p>");
    Results = replaceAll(Results, "</haco_opportunity_list>", "</table>");
    Results = replaceAll(Results, "</haco_opportunity_item>", "</tr>");
    Results = replaceAll(Results, "</haco_opportunity_number>", "</td>");
    Results = replaceAll(Results, "':</haco_opportunity_function>", "</td>");
    Results = replaceAll(Results, "</haco_opportunity_defect>", "</td>");
    Results = replaceAll(Results, "</haco_opportunity_suggestion>", "</td>");
    Results = replaceAll(Results, "</haco_opportunity_extrainfo>", "</td>");

    HTMLFile << Results;
    HTMLFile.close();
    break;
  }
  case OutputFormat::CLI:
  default:
    Results = replaceAll(Results, "<haco_header_big>", "");
    Results = replaceAll(Results, "<haco_header_medium>", "");
    Results = replaceAll(Results, "<haco_header_small>", "");
    Results = replaceAll(Results, "<haco_text>", "");
    Results = replaceAll(Results, "<haco_opportunity_list>", "");
    Results = replaceAll(Results, "<haco_opportunity_item>", "");
    Results = replaceAll(Results, "<haco_opportunity_number>", "");
    Results = replaceAll(Results, "<haco_opportunity_function>", "");
    Results = replaceAll(Results, "<haco_opportunity_defect>", "");
    Results = replaceAll(Results, "<haco_opportunity_suggestion>", "");
    Results = replaceAll(Results, "<haco_opportunity_extrainfo>", "");
    Results = replaceAll(Results, "</haco_header_big>", "");
    Results = replaceAll(Results, "</haco_header_medium>", "");
    Results = replaceAll(Results, "</haco_header_small>", "");
    Results = replaceAll(Results, "</haco_text>", "");
    Results = replaceAll(Results, "</haco_opportunity_list>", "");
    Results = replaceAll(Results, "</haco_opportunity_item>", "");
    Results = replaceAll(Results, "</haco_opportunity_number>", "");
    Results = replaceAll(Results, "</haco_opportunity_function>", "");
    Results = replaceAll(Results, "</haco_opportunity_defect>", "");
    Results = replaceAll(Results, "</haco_opportunity_suggestion>", "");
    Results = replaceAll(Results, "</haco_opportunity_extrainfo>", "");
    llvm::outs() << Results;
    break;
  }
}

int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  std::string Results;
  raw_string_ostream StringOut(Results);

  LoopPrinter Printer(StringOut);
  MatchFinder Finder;
  Finder.addMatcher(LoopMatcher, &Printer);
  Finder.addMatcher(BreakMatcher, &Printer);

  StringOut << "<haco_header_big>OpenMP Loop Check Results</haco_header_big>";

  StringOut.changeColor(llvm::raw_ostream::Colors::SAVEDCOLOR, true)
      << llvm::formatv("\n<haco_header_medium>{0} refactoring "
                       "opportunities: {1}</haco_header_medium>\n\n",
                       RefactoringCode, RefactoringDescription);

  StringOut.resetColor() << "<haco_text>" << RefactoringRationale
                         << "</haco_text>"
                         << "\n\n";

  StringOut << "<haco_opportunity_list>";

  auto ReturnToolValue = Tool.run(newFrontendActionFactory(&Finder).get());

  StringOut << "</haco_opportunity_list>";

  StringOut << '\n';

  int NumberOfOpportunities = Printer.getNumberOfOpportunities();
  if (NumberOfOpportunities == 0) {
    StringOut.changeColor(llvm::raw_ostream::Colors::RED)
        << "<haco_header_small>No refactoring opportunities were "
           "found.</haco_header_small>";
  } else {
    StringOut << llvm::formatv("<haco_header_small>Number of {0} opportunities "
                               "found: {1}</haco_header_small>\n\n",
                               RefactoringCode, NumberOfOpportunities);
    StringOut.changeColor(llvm::raw_ostream::Colors::RED)
        << "<haco_text>Address these changes to obtain better optimizations in "
           "the "
           "following steps.</haco_text>";
  }

  StringOut.resetColor() << '\n';

  printResults(OutputFormat::HTML, StringOut.str());

  return ReturnToolValue;
}
