configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/Version.h.in"
    "${CMAKE_CURRENT_BINARY_DIR}/Version.h"
)

yo_add_tool(
    yo-cli
    main.cpp
    Version.h
    YO_LIBS yo util
)

target_include_directories(yo-cli PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
set_target_properties(yo-cli PROPERTIES OUTPUT_NAME yo)
target_compile_options(yo-cli PRIVATE "-fvisibility-inlines-hidden")
