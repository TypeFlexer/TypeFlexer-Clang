cmake_minimum_required(VERSION 3.0)

project(Microbenchmarks)

# Set the custom Clang compiler path
set(CMAKE_C_COMPILER /home/arun/Desktop/TypeFlexer-Clang/llvm/cmake-build-debug/bin/clang)

# Compile checked_lib.c into a static library
add_library(checked_lib STATIC ${CMAKE_SOURCE_DIR}/checked_lib.c)

# Compile the main executable access_wasm
add_executable(access_wasm access.c)

# Set the compilation and linking options
target_compile_options(access_wasm PRIVATE -O2 -fwasmsbx -Dwasmsbx)
target_link_options(access_wasm PRIVATE -ldl -lstdc++)

# Link the necessary libraries, including the newly compiled checked_lib library
target_link_libraries(access_wasm
        ${CMAKE_SOURCE_DIR}/libisc_lib_final.a
        ${CMAKE_SOURCE_DIR}/../../sandboxLib/WASM_SBX/libSBX_CON_LIB.a

        checked_lib  # Link the newly created static library
)
