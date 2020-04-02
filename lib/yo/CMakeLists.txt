yo_add_lib(
    yo

    Driver.h
    Driver.cpp
    IRGen.h
    IRGen.cpp
    IRGen+Decl.cpp
    IRGen+Stmt.cpp
    IRGen+Expr.cpp
    Mangling.h
    Mangling.cpp
    NameLookup.h
    NameLookup.cpp
    TemplateSpecialization.h
    TemplateSpecialization.cpp
    Type.h
    Type.cpp
    util_llvm.h

    YO_LIBS lex parse util
    LLVM_LIBS core support native nativecodegen passes
)
