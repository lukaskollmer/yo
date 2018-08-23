//
//  LKYOParser.m
//  yo
//
//  Created by Lukas Kollmer on 06.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

#import "LKYOParser.h"
#import "LKUtilities.h"
#import "yo-Swift.h"

#include "grammar.h"

#include <sys/kdebug_signpost.h>

static BOOL shouldEmitSignposts;


#define rule(name) mpc_parser_t *name = mpc_new(#name)

@implementation LKYOParser {
    mpc_parser_t *_program;
    NSMutableArray<NSString *> *_filepaths;
}

+ (instancetype)sharedInstance {
    static dispatch_once_t once;
    static id sharedInstance;
    dispatch_once(&once, ^{
        sharedInstance = [[self alloc] init];
        shouldEmitSignposts = [LKYOCLI hasFlag:@"-emit-signposts"];
    });
    return sharedInstance;
}


- (instancetype)init {
    self = [super init];
    self->_filepaths = [NSMutableArray array];
    
    [self parseGrammar];
    
    return self;
}


- (void)parseGrammar {
    // TODO group these and put all related ones onto the same line
    rule(ident);
    rule(function);
    rule(program);
    rule(stmt);
    rule(expr);
    rule(ret);
    rule(number);
    rule(composite);
    rule(topLevelStatement);
    rule(type);
    rule(paramList);
    rule(fn_ptr);
    rule(expr_list);
    rule(fn_call);
    rule(binop_add);
    rule(binop_mul);
    rule(lexpr);
    rule(var_decl);
    rule(type_decl);
    rule(impl);
    rule(static_target);
    rule(call_target);
    rule(var_access);
    rule(assignment);
    rule(annotation);
    rule(stmt_fn_call);
    rule(lambda);
    rule(import);
    rule(string);
    rule(protocol);
    rule(typecast);
    rule(array_literal);
    rule(boxed_expr);
    rule(if_stmt);
    rule(else_stmt);
    rule(cond_stmt);
    rule(while_stmt);
    
    rule(comp_op);
    rule(bin_cond_op);
    rule(bin_cond);
    rule(comp);
    rule(lcond);
    rule(cond);
    rule(boolean);
    rule(subscript);
    rule(for_loop);
    rule(range);
    rule(for_loop_target);
    
    rule(unary);
    rule(global_var);
    
    rule(number_double);
    rule(number_b02);
    rule(number_b08);
    rule(number_b10);
    rule(number_b16);
    
    rule(in_place_binop);
    
    rule(asm_stmt);
    rule(enum_decl);
    rule(defer_block);
    rule(function_signature);
    
    
    // TODO make sure these arguments are in the exact same order as the tag declarations in grammar.h !!!
    mpc_err_t *err =
    mpca_lang(MPCA_LANG_DEFAULT, YO_GRAMMAR,
              ident, boolean, number, string, number_double, number_b02, number_b08, number_b10, number_b16,
              fn_ptr, global_var, type_decl, impl, type,
              expr_list, subscript, var_access, static_target, call_target, fn_call, array_literal,
              boxed_expr, range, binop_mul, binop_add, unary, lambda, typecast, lexpr, expr,
              ret, var_decl, in_place_binop, assignment, stmt_fn_call,
              comp_op, bin_cond_op, bin_cond, comp, lcond, cond, if_stmt, else_stmt, while_stmt, cond_stmt, for_loop_target, for_loop,
              asm_stmt, defer_block, stmt, composite, paramList,
              import, protocol, enum_decl, function_signature, annotation, function, topLevelStatement, program, NULL);
    
    if (err) {
        mpc_err_print(err);
        mpc_err_delete(err);
        [NSException raise:@"ugh" format:@"fuck"];
    }
    mpc_optimise(program);
    
    self->_program = program;
}


- (mpc_ast_t *)parseFileAtPath:(NSString *)filePath {
    if (shouldEmitSignposts) kdebug_signpost_start(101, self->_filepaths.count, 0, 0, 0);
    
    [_filepaths addObject:filePath];
    
    NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    input = [self preprocessInput:input];
    
    mpc_result_t r;
    if (mpc_parse("input", input.UTF8String, self->_program, &r)) {
        if (shouldEmitSignposts) kdebug_signpost_end(101, 0, 0, 0, 0);
        
        return (mpc_ast_t *)r.output;
    }
    
    NSLog(@"Error parsing %@", filePath);
    mpc_err_print(r.error);
    mpc_err_delete(r.error);
    return NULL;
}


- (void)freeAST:(mpc_ast_t *)ast {
    mpc_ast_delete(ast);
}

- (void)_printFilePathIndexes {
    if (!shouldEmitSignposts) return;
    [_filepaths enumerateObjectsUsingBlock:^(NSString * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        NSLog(@"%lu - %@", (unsigned long)idx, obj);
    }];
}



// Pre-process the input string
// TODO:
// - [ ] replace all lines that only contain a comment w/ an empty line
// - [ ] trim all line comments that are at the end of a line which also contains something else before the comment
// - [ ] replace all inline block comments w/ the appropriate number of whitespace
// It's important that removing the comments does not change the line/column info!
- (NSString *)preprocessInput:(NSString *)input {
    return [self stringByRemovingCommentsInString:input];
}





- (NSString *)stringByRemovingCommentsInString:(NSString *)string {
    NSMutableArray *retval = [NSMutableArray array];
    
    for (NSString *line in [string componentsSeparatedByString:@"\n"]) {
        NSString *lineWithWhitespaceTrimmed = [line stringByTrimmingWhitespace];
        
        if (![lineWithWhitespaceTrimmed hasPrefix:@"//"]) {
            [retval addObject:line];
        } else {
            [retval addObject:@""]; // insert an empty line instead of the comment to make sure line numbers stay the same
        }
        
    }
    
    return [retval componentsJoinedByString:@"\n"];
}

@end
