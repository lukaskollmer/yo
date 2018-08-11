//
//  grammar.h
//  yo
//
//  Created by Lukas Kollmer on 09.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

#pragma once


// TODO make sure these are in the exact same order as the parameters in the `mpca_lang` call !!!


const char *YO_GRAMMAR =
" ident         : /[a-zA-Z_][a-zA-Z0-9_]*/;  "
" boolean       : (\"true\" | \"false\") ;"
//" number      : /[0-9]+/ ; "
" number        : <number_b02> | <number_b08> | <number_b16> | <number_b10> ; " // <number_b10> has to be last bc otherwise, it'd parse the other bases' leading 0 as a single base 10 literal
" string        : /\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\"/ ;                   "
// [string] we have to include the quotes as part of the regex to peoperly capture whitespace at the beginning/end of the string literal


// Number literals
" number_b02    : /0b[01]+/ ; "
" number_b08    : /0[0-7]+/ ; "
" number_b10    : /[0-9]+/ ; "
" number_b16    : /0x[0-9a-f]+/ ; " // TODO also allow uppercase letters? (either everything upper- or everything lowercased!)

" fn_ptr        : \"fn\" '<' '(' (<type> (',' <type>)* )? ')' ':' <type> '>' ; "

" global_var    : \"static\" <ident> ':' <type>  ('=' <expr>)?   ';'   ; "  // TODO remove the type requirement?

" type_decl     : (\"type\" | \"struct\" ) <ident> '(' <paramList>? ')' ';'   ;        "


" impl          : \"impl\" <ident> '{' <function>* '}' ;             "


" type          : <fn_ptr>                                                          "
"               | <ident> ;                                                         "

" expr_list     : <expr> ( ',' <expr> )*  ;          "

" subscript     : <var_access> '[' <expr> ']' ;  "

" var_access    : <ident> ( '.' <ident> )* ;      "

" static_target : <ident> \"::\" <ident> ; "

" call_target   : ( <static_target> | <var_access> ) ;  "

// TODO allow `Foo::bar().x` et al (`Foo::bar().x().y.z.a()`, etc)
" fn_call       : <call_target> '(' <expr_list>? ')'    ;    "

" array_literal : ('[' | '{') <expr_list>? (']' | '}') ; "

" boxed_expr    : '@' <expr> ;"

" range         : <expr> (\"...\" | \"..<\") <expr> ; "


" binop_mul     : <lexpr> (( '*' | '/' | '%' ) (<binop_mul> | <lexpr>))+  ;   "

" binop_add     : (<binop_mul> | <lexpr>) (( '+' | '-' | '&' | '|' | \"<<\" | \">>\" | '^') (<binop_add> | <binop_mul> | <lexpr>))+ ; "

" unary         : ('-' | '~' | '!') <expr>  ;  "


// todo allow lambda param types (ie `|x: int| -> {}`)
" lambda        : '|' (<ident> (':' <type>)? (',' <ident> (':' <type>)?)*)? '|' \"->\"  <composite>      ;           "

" typecast      : <lexpr> \"as\" <type>   ;  "


" lexpr         : <boolean>                             "
"               | <unary>                       "
"               | '(' <expr>  ')'                       "
"               | <number>                       "
"               | <string>                       "
"               | <array_literal>                       "
// TODO can <fn_call> be moved to <expr> ?
"               | <fn_call>                       "
"               | <subscript>                       "
"               | <lambda>                       "
"               | <static_target>                       " // static_target in a <lexpr> is an enum case
"               | <var_access>    ;                     "


" expr          : <binop_add>                                                          "
"               | <binop_mul>                                                           "
"               | <typecast>                                                           "
"               | <boxed_expr>                                                           "
//"             | <cond>   " // TODO is this a good idea? TEST
"               | <lexpr>                                                           "
"               | \"nil\" ;                                                         "

" ret           : \"ret\" <expr>? ';' ; "

" var_decl      : \"val\" <ident> (':' <type>)? ('=' <expr>)? ';' ; "

// TODO split up the various kinds of assignments? (attribute, subscript, etc)
" in_place_binop    : (\"+=\" | \"-=\" | \"*=\" | \"/=\" | \"%=\" | \"|=\" | \"&=\" | \"^=\" | \">>=\" | \"<<=\")  ;    "
" assignment    : <var_access> ( '[' <expr> ']' )? ('=' | <in_place_binop> ) <expr>  ';' ;             "

// Q: Why is stmt_fn_call its own thing, instead of replacing the respective line in stmt with `<fn_call> ';'` ?
// A: We have to make sure the semicolon is part of the statement function call, which is important to have them both put in the same ast node
" stmt_fn_call  : <fn_call> ';'  ; "


" comp_op       : \"==\" | \"!=\" | \"<=\" | \">=\" | '<' | '>'  ; "
" bin_cond_op   : \"&&\" | \"||\" ;  "

" bin_cond      : <lcond> <bin_cond_op> <lcond>  ; "

" comp          : <expr> <comp_op> <expr>   ; " // TODO expr or lexpr?

// a part of a condition that can evaluate to true
" lcond         : <comp>   "
"               | <expr>  ; "

" cond          : <bin_cond>+     "
"               | <lcond> ; "


" if_stmt       : \"if\" <cond> <composite>  <else_stmt>?   ;         "
" else_stmt     : \"else\" ( <if_stmt> | <composite> )    ;         "

" while_stmt    : \"while\" <cond> <composite>     ;         "


" cond_stmt     : <if_stmt> | <while_stmt> ; "

" for_loop_target   : <range>  ; "

" for_loop      : \"for\" <ident> \"in\" <for_loop_target> <composite> ; "


" asm_stmt      : \"__asm\" '(' <ident> ',' (<number> | <string>) ')' ';'  ;   "

" stmt          : <ret>             "
"               | <asm_stmt>             "
"               | <stmt_fn_call>    "
"               | <cond_stmt>       "
"               | <for_loop>        "
"               | <assignment>      "
"               | <var_decl> ;      "
// var_decl at the end because it can interfer w/ other statements
// example: `value = 12;` would be parsed as `val ue = 12;`

" composite     : '{' <stmt>* '}' ;   "

" paramList     : <ident> ':' <type> (',' <ident> ':' <type> )* ; "

" import        : \"use\" <string> ';'  ;  "

" protocol      : <annotation>? \"protocol\" <ident> '{' <function>* '}' ; "

" enum_decl     : \"enum\" <ident> '{' <ident> ( ',' <ident> )* '}' ; "

" annotation    : \"#[\" <ident> ( ',' <ident> )* ']' ; "

" function      : <annotation>* \"static\"? \"fn\" <ident> '(' <paramList>? ')' ':' <type> <composite> ;      "

" topLevelStatement : ( <import> | <global_var> | <protocol> | <enum_decl> | <function> | <type_decl> | <impl> ) ; "

" program       : /^/  <topLevelStatement>* /$/;               ";
