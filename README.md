# OpenMP Loop Check

OpenMP Loop Check is a Clang utility for verifying if **for** loops in C/C++ files can be parallelized with OpenMP. Currently, it reports the following:

- Loops without init section.
- Loops without condition section.
- Loops without increment section.
- Loops with 'break' statements in the body

## Build Instructions

Download the LLVM sources: 
```bash
git clone https://github.com/llvm/llvm-project.git
cd llvm-project
```
Clone this repository into the **clang-tools-extra** folder of the LLVM sources:
```bash
cd clang-tools-extra
git clone git@github.com:American-Chip/omp-loop-checker.git
```
Add **omp-loop-check** as a subdirectory into the `CMakeLists.txt` file of **clang-tools-extra**:
```CMake
add_subdirectory(omp-loop-check)
```
Configure LLVM build with **clang** and **clang-tools-extra** projects enabled. E.g:
```Bash
cd ..
cmake -S llvm -B build -G Ninja -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" -DCMAKE_BUILD_TYPE=Release
```
Build **omp-loop-check**. E.g.:
```Bash
cd build
ninja omp-loop-check
```

## Usage

Now you can execute the **omp-loop-check** utility from the LLVM build folder.
```Bash
bin/omp-loop-check <C/C++ file> --
```
You can also analyze more then one file:
```Bash
bin/omp-loop-check <C/C++ file 1> <C/C++ file 2> <C/C++ file 3> --
```
You can add headers and macro definitions in two mutually exclusive ways:

1. Writing them explicitely using **--** at the end of the command:
    ```Bash
    bin/omp-loop-check <C/C++ file> -- [-I<header folder> -D<macro>]
    ```
2. Using the **-p** option to specify the containing folder of a project configuration stored on a **compile_commands.json** file:
    ```Bash
    bin/omp-loop-check <C/C++ file> -p <compile_commands.json folder>
    ```
