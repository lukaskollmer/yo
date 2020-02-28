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

yo_add_tool(
    yo_test
    mangling.cpp
    LINK_YO
)
# add_executable(
#     yo_test
#     mangling.cpp
# )
# target_include_directories(yo_test PRIVATE "src/")
add_dependencies(yo_test gtest test_mangling_input_gen)
target_link_libraries(yo_test gtest_main)
add_test(yo_test yo_test)