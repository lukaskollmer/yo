yo_add_tool(
    yo-demangle
    main.cpp

    YO_LIBS yo util
    LLVM_LIBS support
)

# target_link_libraries(yo-demangle yo)
# target_include_directories(yo-demangle PRIVATE "${PROJECT_SOURCE_DIR}/lib")
