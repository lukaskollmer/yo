cmake_minimum_required(VERSION 3.11)

project(yo)

set(YO_VERSION "0.0.1")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_BUILD_TYPE Debug)

# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")

option(BUILD_TESTS "Build tests" OFF)


# find_package(LLVM 9 REQUIRED CONFIG)
find_package(LLVM 8 REQUIRED PATHS /usr/local/opt/llvm@8)
add_definitions(${LLVM_DEFINITIONS})
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in ${LLVM_DIR}")


# set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/${CMAKE_BUILD_TYPE}/lib)
# set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/${CMAKE_BUILD_TYPE}/lib)
# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/${CMAKE_BUILD_TYPE}/bin)


macro(yo_add_target)
    cmake_parse_arguments(
        ARG                 # output variable prefix
        ""                  # boolean arguments
        "TYPE"              # single-value arguments
        "YO_LIBS;LLVM_LIBS" # multi-value arguments
        ${ARGN}             # input
    )
    set(target_type ${ARG_TYPE})
    list(GET ARG_UNPARSED_ARGUMENTS 0 target_name)
    list(SUBLIST ARG_UNPARSED_ARGUMENTS 1 -1 src_files)

    if(${target_type} STREQUAL "lib")
        add_library(${target_name} ${src_files})
    elseif(${target_type} STREQUAL "bin")
        add_executable(${target_name} ${src_files})
        set(IS_BIN TRUE)
    else()
        message(FATAL_ERROR "Invalid target type '${target_type}'")
    endif()

    llvm_map_components_to_libnames(llvm_libs ${ARG_LLVM_LIBS})
    target_include_directories(${target_name} PRIVATE SYSTEM ${LLVM_INCLUDE_DIRS})
    target_link_libraries(${target_name} ${llvm_libs})
    
    foreach(libname ${ARG_YO_LIBS})
        target_link_libraries(${target_name} ${libname})
    endforeach()
    target_include_directories(${target_name} PRIVATE "${PROJECT_SOURCE_DIR}/lib")
endmacro(yo_add_target)


macro(yo_add_lib)
    yo_add_target(TYPE lib ${ARGN})
endmacro(yo_add_lib)

macro(yo_add_tool)
    yo_add_target(TYPE bin ${ARGN})
endmacro(yo_add_tool)


add_subdirectory(lib/lex)
add_subdirectory(lib/parse)
add_subdirectory(lib/util)
add_subdirectory(lib/yo)
add_subdirectory(tools/yo-cli)
add_subdirectory(tools/yo-demangle)


if(BUILD_TESTS)
    add_subdirectory(test)
endif()
