cmake_minimum_required(VERSION 3.0)

project(MyProject)

set(CMAKE_C_COMPILER /home/arun/Desktop/TypeFlexer-Clang/llvm/cmake-build-debug/bin/clang)
set(CMAKE_CXX_COMPILER /home/arun/Desktop/TypeFlexer-Clang/llvm/cmake-build-debug/bin/clang)

add_executable(access_wasm access.c)
target_compile_options(access_wasm PRIVATE -O2 -fwasmsbx -DWASM_SBX)
target_link_options(access_wasm PRIVATE -ldl -lstdc++)
target_link_libraries(access_wasm ${CMAKE_SOURCE_DIR}/../../sandboxLib/WASM_SBX/libSBX_CON_LIB.a ${CMAKE_SOURCE_DIR}/../../sandboxLib/WASM_SBX/libisc_lib_final.a )

add_executable(access_hoard access.c access.h)
target_compile_options(access_hoard PRIVATE -O2 -fheapsbx -DHEAP_SBX)
target_link_options(access_hoard PRIVATE -ldl -lstdc++)
target_link_libraries(access_hoard
        ${CMAKE_SOURCE_DIR}/../../sandboxLib/HoardLib/libhoard.so
        ${CMAKE_SOURCE_DIR}/../../sandboxLib/HoardLib/libmemprofile.a
       )

add_executable(benchmark benchmarkO2.c benchmarkO2.h)
target_compile_options(benchmark PRIVATE -g -O2)
target_link_options(benchmark PRIVATE )
