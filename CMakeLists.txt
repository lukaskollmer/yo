cmake_minimum_required(VERSION 3.13)
project(yo)

set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# TODO split this up into proper subfolders and shit
add_executable(yo
        src/main.cpp
)

llvm_map_components_to_libnames(llvm_libs core support irreader mcjit orcjit native nativecodegen)

target_link_libraries(yo ${llvm_libs})
