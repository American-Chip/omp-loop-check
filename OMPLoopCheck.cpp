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

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

// Refactoring details
static StringRef RefactoringCode = "HACO001";
static StringRef RefactoringDescription = "Loop has missing sections";
static StringRef RefactoringRationale =
    "In order to parallelize a loop with OpenMP, the loop has to be in "
    "canonical form. One of the conditions of the canonical form is that the "
    "loop has to have an init, a condition and an increment section.";

// AST Matchers

// Match a loop inside a function
StatementMatcher LoopMatcher =
    forStmt(hasAncestor(functionDecl().bind("function"))).bind("forLoop");

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

static void emitInitSuggestionDiagnostic(TextDiagnostic &TD,
                                         SourceManager &Source,
                                         const VarDecl &IncVarDecl) {
  TD.printDiagnosticMessage(
      llvm::outs(), true,
      llvm::formatv(
          "Init '{0}' inside the loop header. Follow its "
          "usage from the reported declaration point to pick the right "
          "value. If the variable value does not depend on previous "
          "operations and it is not needed after the loop, "
          "better consider declaring it inside the loop header.",
          IncVarDecl.getName())
          .str(),
      0, 0, true);

  std::optional<PresumedLoc> DeclLocation =
      getPresumedLocation(IncVarDecl.getBeginLoc(), Source);
  if (DeclLocation) {
    TD.emitDiagnostic(FullSourceLoc(IncVarDecl.getBeginLoc(), Source),
                      DiagnosticsEngine::Remark,
                      "declaration of loop variable here", std::nullopt,
                      std::nullopt);
  }

  llvm::outs().resetColor() << '\n';
}

static void emitOpportunityInfo(int CurrentOpportunityNumber,
                                StringRef FunctionName) {
  llvm::outs().changeColor(llvm::raw_ostream::Colors::CYAN, true)
      << llvm::formatv("Opportunity #{0} at function '{1}':",
                       CurrentOpportunityNumber, FunctionName);

  llvm::outs().resetColor() << '\n';
}

static void emitMissingPart(SourceLocation Loc, SourceManager &Source,
                            TextDiagnostic &TD, StringRef MissingPart) {
  std::string Message = "Loop without ";
  Message += MissingPart;
  TD.emitDiagnostic(FullSourceLoc(Loc, Source), DiagnosticsEngine::Warning,
                    Message, std::nullopt, std::nullopt);
}

static void reportLoopWithoutInit(int CurrentOpportunityNumber,
                                  const ForStmt &FS, StringRef FunctionName,
                                  SourceManager &Source,
                                  const LangOptions &LangOpts,
                                  DiagnosticOptions &DiagnosticOpts) {
  TextDiagnostic TD(llvm::outs(), LangOpts, &DiagnosticOpts);

  emitOpportunityInfo(CurrentOpportunityNumber, FunctionName);

  emitMissingPart(FS.getForLoc(), Source, TD, "init");

  // Skip on absence of increment variable
  const VarDecl *IncVarDecl = matchLHSVarDeclFromOperator(FS.getInc());
  if (!IncVarDecl) {
    return;
  }

  // Skip on absence of conforming condition
  const BinaryOperator *CondBO = getExprAsBinaryOperator(FS.getCond());
  if (!CondBO) {
    return;
  }

  // Increment and LHS condition variable are the same, suggest changes
  const VarDecl *CondLHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getLHS());
  if (CondLHSVarDecl &&
      IncVarDecl->getCanonicalDecl() == CondLHSVarDecl->getCanonicalDecl()) {
    emitInitSuggestionDiagnostic(TD, Source, *IncVarDecl);
    return; // Skip further checks
  }

  // Increment and RHS condition variable are the same, suggest changes
  const VarDecl *CondRHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getRHS());
  if (CondRHSVarDecl &&
      IncVarDecl->getCanonicalDecl() == CondRHSVarDecl->getCanonicalDecl()) {
    emitInitSuggestionDiagnostic(TD, Source, *IncVarDecl);
    return; // Skip further checks
  }

  // Increment and condition variable are different, make a warning
  if (CondLHSVarDecl || CondRHSVarDecl) {
    TD.emitDiagnostic(FullSourceLoc(FS.getLParenLoc(), Source),
                      DiagnosticsEngine::Remark,
                      "The condition and the increment variables are not "
                      "the same. Is this intentional?",
                      CharSourceRange::getTokenRange(
                          SourceRange(FS.getLParenLoc(), FS.getRParenLoc())),
                      std::nullopt);

    llvm::outs().resetColor() << '\n';
  }
}

static void reportLoopWithoutCondition(int CurrentOpportunityNumber,
                                       const ForStmt &FS,
                                       StringRef FunctionName,
                                       SourceManager &Source,
                                       const LangOptions &LangOpts,
                                       DiagnosticOptions &DiagnosticOpts) {
  TextDiagnostic TD(llvm::outs(), LangOpts, &DiagnosticOpts);

  emitOpportunityInfo(CurrentOpportunityNumber, FunctionName);

  emitMissingPart(FS.getForLoc(), Source, TD, "condition");

  // Skip on absence of increment variable
  const VarDecl *IncVarDecl = matchLHSVarDeclFromOperator(FS.getInc());
  if (!IncVarDecl) {
    return;
  }

  // Skip on absence of init binary operator
  const BinaryOperator *InitBO =
      llvm::dyn_cast_or_null<BinaryOperator>(FS.getInit());
  if (!InitBO) {
    return;
  }

  // Increment and LHS init variable are the same, suggest changes
  const VarDecl *InitVarDecl = matchVarDeclFromDeclRefExpr(InitBO->getLHS());
  if (InitVarDecl &&
      IncVarDecl->getCanonicalDecl() == InitVarDecl->getCanonicalDecl()) {
    TD.printDiagnosticMessage(
        llvm::outs(), true,
        "Init and increment variables match, but condition variable is "
        "missing.",
        0, 0, true);

    llvm::outs().resetColor() << '\n';
    return; // Skip further checks
  }

  // Increment and init variable are different, make a warning
  if (InitVarDecl) {
    TD.emitDiagnostic(FullSourceLoc(FS.getLParenLoc(), Source),
                      DiagnosticsEngine::Remark,
                      "The init and the increment variables are not "
                      "the same. Is this intentional?",
                      CharSourceRange::getTokenRange(
                          SourceRange(FS.getLParenLoc(), FS.getRParenLoc())),
                      std::nullopt);

    llvm::outs().resetColor() << '\n';
  }
}

static void reportLoopWithoutIncrement(int CurrentOpportunityNumber,
                                       const ForStmt &FS,
                                       StringRef FunctionName,
                                       SourceManager &Source,
                                       const LangOptions &LangOpts,
                                       DiagnosticOptions &DiagnosticOpts) {
  TextDiagnostic TD(llvm::outs(), LangOpts, &DiagnosticOpts);

  emitOpportunityInfo(CurrentOpportunityNumber, FunctionName);

  emitMissingPart(FS.getForLoc(), Source, TD, "increment");

  // Skip on absence of init binary operator
  const BinaryOperator *InitBO =
      llvm::dyn_cast_or_null<BinaryOperator>(FS.getInit());
  if (!InitBO) {
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
    return;
  }

  // Init and LHS condition variable are the same, suggest changes
  const VarDecl *CondLHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getLHS());
  if (CondLHSVarDecl &&
      InitVarDecl->getCanonicalDecl() == CondLHSVarDecl->getCanonicalDecl()) {
    TD.printDiagnosticMessage(
        llvm::outs(), true,
        "Init and condition variables match, but increment variable is "
        "missing.",
        0, 0, true);

    llvm::outs().resetColor() << '\n';
    return; // Skip further checks
  }

  // Init and RHS condition variable are the same, suggest changes
  const VarDecl *CondRHSVarDecl = matchVarDeclFromDeclRefExpr(CondBO->getRHS());
  if (CondRHSVarDecl &&
      InitVarDecl->getCanonicalDecl() == CondRHSVarDecl->getCanonicalDecl()) {
    TD.printDiagnosticMessage(
        llvm::outs(), true,
        "Init and condition variables match, but increment variable is "
        "missing.",
        0, 0, true);

    llvm::outs().resetColor() << '\n';
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

    llvm::outs().resetColor() << '\n';
  }
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

public:
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

    // Report a loop without init
    if (!FS->getInit()) {
      NumberOfOpportunities++;

      // Report a loop without init refactoring opportunity
      reportLoopWithoutInit(NumberOfOpportunities, *FS, FunctionName, Source,
                            Context->getLangOpts(),
                            Context->getDiagnostics().getDiagnosticOptions());
    }

    // Report loop without condition
    if (!FS->getCond()) {
      NumberOfOpportunities++;

      // Report a loop without init refactoring opportunity
      reportLoopWithoutCondition(
          NumberOfOpportunities, *FS, FunctionName, Source,
          Context->getLangOpts(),
          Context->getDiagnostics().getDiagnosticOptions());
    }

    // Report loop without increment
    if (!FS->getInc()) {
      NumberOfOpportunities++;

      // Report a loop without init refactoring opportunity
      reportLoopWithoutIncrement(
          NumberOfOpportunities, *FS, FunctionName, Source,
          Context->getLangOpts(),
          Context->getDiagnostics().getDiagnosticOptions());
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

  LoopPrinter Printer;
  MatchFinder Finder;
  Finder.addMatcher(LoopMatcher, &Printer);

  llvm::outs().changeColor(llvm::raw_ostream::Colors::SAVEDCOLOR, true)
      << llvm::formatv("\n{0} refactoring opportunities: {1}\n\n",
                       RefactoringCode, RefactoringDescription);

  llvm::outs().resetColor() << RefactoringRationale << "\n\n";

  auto ReturnToolValue = Tool.run(newFrontendActionFactory(&Finder).get());

  int NumberOfOpportunities = Printer.getNumberOfOpportunities();
  if (NumberOfOpportunities == 0) {
    llvm::outs().changeColor(llvm::raw_ostream::Colors::RED)
        << "No refactoring opportunities were found.";
  } else {
    llvm::outs() << llvm::formatv("Number of {0} opportunities found: {1}\n\n",
                                  RefactoringCode, NumberOfOpportunities);
    llvm::outs().changeColor(llvm::raw_ostream::Colors::RED)
        << "Address these changes to obtain better optimizations in the "
           "following steps.\n";
  }

  return ReturnToolValue;
}
