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

set_target_properties(yo-cli PROPERTIES OUTPUT_NAME yo)
target_compile_options(yo-cli PRIVATE "-fvisibility-inlines-hidden")
