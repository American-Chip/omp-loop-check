set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omp-loop-check
  OMPLoopCheck.cpp
  )
target_link_libraries(omp-loop-check
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  )
  
