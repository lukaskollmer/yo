find_package(PythonInterp 3.6 REQUIRED)

add_custom_target(
    gen_stdlib
    COMMAND ${PYTHON_EXECUTABLE} "${PROJECT_SOURCE_DIR}/tools/stdlib_gen.py" "${PROJECT_SOURCE_DIR}" "${PROJECT_SOURCE_DIR}/stdlib"
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
