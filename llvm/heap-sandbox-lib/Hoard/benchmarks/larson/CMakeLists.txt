cmake_minimum_required(VERSION 3.5)

# Set the name of the target executable
set(TARGETS larson larson-hoard)

# Add the source files for the targets
add_executable(larson larson.cpp ../../src/source/libhoard.cpp)
add_executable(larson-hoard larson.cpp ../../src/Heap-Layers/wrappers/wrapper.cpp)

# Set the compiler and its flags
set(CMAKE_CXX_COMPILER g++)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -lpthread")

# Link the Hoard library for the "larson-hoard" target
target_link_libraries(larson-hoard ${CMAKE_SOURCE_DIR}/../../src/libhoard.so)

# Specify the output names for the targets
set_target_properties(larson PROPERTIES OUTPUT_NAME "larson")
set_target_properties(larson-hoard PROPERTIES OUTPUT_NAME "larson-hoard")

# Set the installation targets
install(TARGETS ${TARGETS}
        RUNTIME DESTINATION bin)
