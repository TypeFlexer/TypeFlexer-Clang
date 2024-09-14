# Set minimum CMake version
cmake_minimum_required(VERSION 3.13)

# Project name and languages
project(profile LANGUAGES CXX)

# Set C++ Standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Set compiler flags (Ensure consistency with Hoard)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2 -static")

# Add the subdirectory where Hoard's CMakeLists.txt is located
add_subdirectory(Hoard/src)

# Add the CFIProfiler library target
add_library(profile STATIC CFIProfiler.cpp)

# Set target compiler flags
target_compile_options(profile PUBLIC "-fPIC")

# Specify the output name of the library
set_target_properties(profile PROPERTIES OUTPUT_NAME "profile")

# Set the library's include directories (if any)
target_include_directories(profile PUBLIC .)

# Add the Memory library target
add_library(memprofile STATIC MemoryProfiler.cpp)

# Specify the output name of the library
set_target_properties(memprofile PROPERTIES OUTPUT_NAME "memprofile")

# Set the library's include directories (if any)
target_include_directories(memprofile PUBLIC .)

# Make sure hoard_static and hoard_shared are built when building profile or memprofile
add_dependencies(profile build_hoard_shared)
add_dependencies(memprofile build_hoard_shared)

# Define the destination directory for libmemprofile.a
set(MEMPROFILE_DESTINATION "${CMAKE_SOURCE_DIR}/../sandboxLib/HoardLib")

# Ensure the destination directory exists
file(MAKE_DIRECTORY ${MEMPROFILE_DESTINATION})

# Add a post-build command to copy libmemprofile.a to HoardLib directory
add_custom_command(TARGET memprofile POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:memprofile> ${MEMPROFILE_DESTINATION}/libmemprofile.a
        COMMENT "Copying libmemprofile.a to HoardLib directory"
)
