find_package(PythonInterp 3.6 REQUIRED)

add_custom_target(
    gen_stdlib
    COMMAND ${PYTHON_EXECUTABLE} "${PROJECT_SOURCE_DIR}/tools/stdlib_gen.py" "${PROJECT_SOURCE_DIR}" "${PROJECT_SOURCE_DIR}/stdlib"
)

yo_add_lib(
    yo

    AST.h AST.cpp
    ASTVisitor.h ASTVisitor.cpp
    Attributes.h Attributes.cpp
    Diagnostics.h Diagnostics.cpp
    Driver.h Driver.cpp
    IRGen.h IRGen.cpp IRGen+Decl.cpp IRGen+Stmt.cpp IRGen+Expr.cpp
    Lexer.h Lexer.cpp
    Mangling.h Mangling.cpp
    NamedScope.h
    Parser.h Parser.cpp
    StdlibResolution.h StdlibResolution.cpp
    TemplateSpecialization.h TemplateSpecialization.cpp
    Token.h Token.cpp
    Type.h Type.cpp
    TypeDesc.h TypeDesc.cpp

    util.h util.cpp
    util_llvm.h

    LLVM_LIBS core support native nativecodegen passes
)

add_dependencies(yo gen_stdlib)
