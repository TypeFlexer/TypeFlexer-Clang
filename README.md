## Testing out TypeFlexer

Programmers are welcome to use TypeFlexer as it is being implemented.  You will
have to build your own copy of the compiler.
Below are the directions on how to do it.

Step 1: Switch to work Directory
```
cd <WORK_DIR>
```

Step 2: Clone the repository
```
git clone https://github.com/TypeFlexer/TypeFlexer-Clang.git
```

Step 3: Now create a build directory generate the Cmake files

```
cd <WORK_DIR>/Typeflexer-Clang/llvm/
mkdir build
cd build 
cmake -G Ninja -DLLVM_ENABLE_PROJECTS=clang -DLLVM_ENABLE_RUNTIMES=compiler-rt -DCMAKE_LINKER=/usr/bin/gold DCMAKE_BUILD_TYPE=Debug -DLLVM_LIT_ARGS=-v -DLLVM_PARALLEL_LINK_JOBS=1 ../
```

Step 6: Now you are all set to build the target
Execute any of the following command (based on your requirement) in the build directory
```
ninja clang //this command will only build the compiler
ninja checkcbox-headers // This command will run all the sanity test cases for TypeFlexer project 
```

Step 5 (Optional) Sanitizers -->
```
First compile target llvm-config
cd <compiler-rt> // Switch to compiler-rt directory
cmake .
make install
export ASAN_SYMBOLIZER_PATH=<pathToLLVMSymbolizer> // For meaningful ASAN backtrace
```
## Testing Playground for TypeFlexer

```
cd <WORK_DIR>/Typeflexer-Clang/tests
```

## Sandbox Libraries

```
cd <WORK_DIR>/Typeflexer-Clang/sandboxLib
```

