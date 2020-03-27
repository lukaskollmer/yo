find_package(PythonInterp 3.6 REQUIRED)

add_custom_target(
    gen_stdlib
    COMMAND
        ${PYTHON_EXECUTABLE}
        "${PROJECT_SOURCE_DIR}/tools/stdlib_gen.py"
        "${CMAKE_SOURCE_DIR}/stdlib"
        "${CMAKE_CURRENT_BINARY_DIR}/stdlib_sources.cpp"
)

yo_add_lib(
    parse

    AST.h AST.cpp
    ASTVisitor.h ASTVisitor.cpp
    Attributes.h Attributes.cpp
    TypeDesc.h TypeDesc.cpp
    Parser.h Parser.cpp
    StdlibResolution.h StdlibResolution.cpp

    YO_LIBS lex util yo
)

add_dependencies(parse gen_stdlib)
target_include_directories(parse PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
