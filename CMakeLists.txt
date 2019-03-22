cmake_minimum_required(VERSION 3.10)
project(yo)

set(YO_VERSION "0.0.1")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_BUILD_TYPE Debug)

find_package(LLVM 7 REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in ${LLVM_DIR}")


configure_file(
  "${PROJECT_SOURCE_DIR}/src/Version.h.in"
  "${PROJECT_SOURCE_DIR}/src/Version.h"
)
include_directories("${PROJECT_BINARY_DIR}")



# TODO split this up into proper subfolders and shit
add_executable(yo
        src/CommandLine.h
        src/CommandLine.cpp
        src/CharacterSet.h
        src/CharacterSet.cpp
        src/Token.h
        src/Token.cpp
        src/Lexer.h
        src/Lexer.cpp
        src/AST.h
        src/AST.cpp
        src/Parser.h
        src/Parser.cpp
        src/TypeInfo.h
        src/TypeInfo.cpp
        src/Mangling.h
        src/Mangling.cpp
        src/Scope.h
        src/Scope.cpp
        src/TypeCache.h
        src/TypeCache.cpp
        src/IRGen.h
        src/IRGen.cpp
        src/JIT.h
        src/JIT.cpp
        src/util.h
        src/util.cpp
        src/util_llvm.h
        src/main.cpp
)

#llvm_map_components_to_libnames(llvm_libs core support irreader mcjit orcjit native nativecodegen)
llvm_map_components_to_libnames(llvm_libs all)

target_link_libraries(yo ${llvm_libs})
target_compile_options(yo PRIVATE "-fvisibility=hidden")
