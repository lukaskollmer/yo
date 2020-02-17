cmake_minimum_required(VERSION 3.11)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_BUILD_TYPE Debug)

# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")

option(BUILD_TESTS "Build test programs" OFF)

project(yo)

set(YO_VERSION "0.0.1")
set(YO_STDLIB_PATH "${CMAKE_BINARY_DIR}/stdlib")
string(TIMESTAMP YO_BUILD_DATE "%Y-%m-%d_%H%MZ" UTC)


# find_package(LLVM 9 REQUIRED CONFIG)
find_package(LLVM 8 REQUIRED PATHS /usr/local/opt/llvm@8)
find_package(PythonInterp 3.6 REQUIRED)

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


add_library(yo
    src/AST.h
    src/AST.cpp
    src/Attributes.h
    src/Attributes.cpp
    src/CommandLine.h
    src/CommandLine.cpp
    src/Diagnostics.h
    src/Diagnostics.cpp
    src/Driver.h
    src/Driver.cpp
    src/IRGen.h
    src/IRGen.cpp
    src/Lexer.h
    src/Lexer.cpp
    src/Mangling.h
    src/Mangling.cpp
    src/NamedScope.h
    src/Parser.h
    src/Parser.cpp
    src/Pygmentize.h
    src/Pygmentize.cpp
    src/StdlibResolution.h
    src/StdlibResolution.cpp
    src/TemplateSpecialization.h
    src/TemplateSpecialization.cpp
    src/Token.h
    src/Token.cpp
    src/Type.h
    src/Type.cpp
    src/TypeDesc.h
    src/TypeDesc.cpp
    src/Version.h

    src/util.h
    src/util.cpp
    src/util_llvm.h
)


add_dependencies(yo gen_stdlib)


# LLVM
llvm_map_components_to_libnames(llvm_libs core support native nativecodegen passes)
target_include_directories(yo PRIVATE SYSTEM ${LLVM_INCLUDE_DIRS})
target_link_libraries(yo ${llvm_libs})


# compilation options
target_compile_options(yo PRIVATE "-fvisibility-inlines-hidden")


add_executable(yo_driver src/main.cpp)
target_link_libraries(yo_driver yo)
set_target_properties(yo_driver PROPERTIES OUTPUT_NAME yo)


# Tests


if(BUILD_TESTS)
    set(TEST_DIR "${CMAKE_BINARY_DIR}/test")
    include(FetchContent)

    enable_testing()

    message(STATUS "Fetching GoogleTest")
    FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG        release-1.10.0
    )
    FetchContent_GetProperties(googletest)

    if(NOT googletest_POPULATED)
        # TODO would be nice if this could somehow display a progress bar or something
        FetchContent_Populate(googletest)
        add_subdirectory(${googletest_SOURCE_DIR} ${googletest_BINARY_DIR})
    endif()

    message(STATUS "Processing test input files")

    add_custom_target(
        test_mangling_input_gen
        COMMAND cat "${TEST_DIR}/mangling_input.txt" | xxd "-i" > "${TEST_DIR}/mangling_input.txt.in"
        BYPRODUCTS "${TEST_DIR}/mangling_input.txt.in"
    )

    add_executable(
        yo_test
        test/mangling.cpp
    )
    target_include_directories(yo_test PRIVATE "src/")
    add_dependencies(yo_test gtest)
    add_dependencies(yo_test test_mangling_input_gen)
    target_link_libraries(yo_test yo gtest_main)
    add_test(yo_test yo_test)
endif()
