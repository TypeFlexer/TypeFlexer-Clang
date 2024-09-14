# Set minimum CMake version
cmake_minimum_required(VERSION 3.13)

# Project name
project(Hoard)

# Define a custom target that invokes the Makefile's Linux-gcc-x86_64 target
add_custom_target(
        build_hoard_shared
        COMMAND make Linux-gcc-x86_64
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/heap-sandbox-lib/Hoard/src
        COMMENT "Building hoard_shared using Makefile target Linux-gcc-x86_64"
)

add_custom_target(
        build_hoard_static
        COMMAND make Linux-gcc-x86_64-static
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/heap-sandbox-lib/Hoard/src
        COMMENT "Building hoard_static using Makefile target Linux-gcc-x86_64-static"
)

# Create an aggregate target to build both hoard_shared and hoard_static
add_custom_target(
        build_hoard ALL
        DEPENDS build_hoard_shared build_hoard_static
)

# Define the destination directory for libhoard.a
set(HOARD_LIB_DESTINATION "${CMAKE_SOURCE_DIR}/../sandboxLib/HoardLib")

# Ensure the destination directory exists
file(MAKE_DIRECTORY ${HOARD_LIB_DESTINATION})

# Add a post-build command to copy libhoard.a to HoardLib directory after build_hoard_static
add_custom_command(TARGET build_hoard_static POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${CMAKE_CURRENT_SOURCE_DIR}/libhoard.a"
        "${HOARD_LIB_DESTINATION}/libhoard.a"
        COMMENT "Copying libhoard.a to sandboxLib directory"
)
