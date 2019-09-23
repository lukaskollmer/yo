cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_BUILD_TYPE Debug)

# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")

project(yo)

set(YO_VERSION "0.0.1")
set(YO_STDLIB_PATH "${CMAKE_BINARY_DIR}/stdlib")
string(TIMESTAMP YO_BUILD_DATE "%Y-%m-%d_%H%MZ" UTC)


find_package(LLVM 8 REQUIRED CONFIG)
find_package(PythonInterp 3.6 REQUIRED)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in ${LLVM_DIR}")


configure_file(
    "${PROJECT_SOURCE_DIR}/src/Version.h.in"
    "${PROJECT_SOURCE_DIR}/src/Version.h"
)


add_custom_target(gen_stdlib
    COMMAND ${PYTHON_EXECUTABLE} tools/stdlib_gen.py "${CMAKE_BINARY_DIR}" "${YO_STDLIB_PATH}"
)


add_executable(yo
    src/main.cpp

    src/AST.h
    src/AST.cpp
    src/Attributes.h
    src/Attributes.cpp
    src/CommandLine.h
    src/CommandLine.cpp
    src/IRGen.h
    src/IRGen.cpp
    src/Lexer.h
    src/Lexer.cpp
    src/Mangling.h
    src/Mangling.cpp
    src/Parser.h
    src/Parser.cpp
    src/Pygmentize.h
    src/Pygmentize.cpp
    src/Scope.h
    src/Scope.cpp
    src/StdlibResolution.h
    src/StdlibResolution.cpp
    src/TemplateResolution.h
    src/TemplateResolution.cpp
    src/Token.h
    src/Token.cpp
    src/Type.h
    src/Type.cpp
    src/TypeDesc.h
    src/TypeDesc.cpp

    src/util.h
    src/util.cpp
    src/util_llvm.h
)

add_dependencies(yo gen_stdlib)



# link LLVM
llvm_map_components_to_libnames(llvm_libs core support native nativecodegen passes)
target_link_libraries(yo ${llvm_libs})

# compilation options
target_compile_options(yo PRIVATE "-fvisibility-inlines-hidden")
