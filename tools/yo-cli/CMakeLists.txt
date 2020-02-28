configure_file(
    Version.h.in
    Version.h
)

yo_add_tool(
    yo-cli
    main.cpp
    Version.h
    LINK_YO
)

# target_link_libraries(yo-cli yo)
# target_include_directories(yo-cli PRIVATE "${PROJECT_SOURCE_DIR}/lib")
set_target_properties(yo-cli PROPERTIES OUTPUT_NAME yo)
target_compile_options(yo-cli PRIVATE "-fvisibility-inlines-hidden")
