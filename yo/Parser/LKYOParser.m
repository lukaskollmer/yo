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
        shouldEmitSignposts = [LKYOCLI hasFlag:[LKYOCLI emitSignpostsFlag]];
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
    rule(lvalue);
    rule(pointer_op);
    rule(lexpr);
    rule(var_decl);
    rule(struct_decl);
    rule(impl);
    rule(static_target);
    rule(call_target);
    rule(var_access);
    rule(assignment);
    rule(annotation);
    rule(stmt_fn_call);
    rule(lambda);
    rule(import);
    rule(character);
    rule(string);
    rule(protocol);
    rule(typecast);
    rule(array_literal);
    rule(boxed_expr);
    rule(if_stmt);
    rule(else_if_stmt);
    rule(else_stmt);
    rule(while_stmt);
    rule(break_stmt);
    rule(continue_stmt);
    
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
    
    rule(const_decl);
    
    rule(varargs_spread);
    
    
    // TODO make sure these arguments are in the exact same order as the tag declarations in grammar.h !!!
    mpc_err_t *err =
    mpca_lang(MPCA_LANG_DEFAULT, YO_GRAMMAR,
              ident, boolean, number, character, string, number_double, number_b02, number_b08, number_b10, number_b16,
              fn_ptr, global_var, const_decl, struct_decl, impl, type,
              expr_list, subscript, var_access, static_target, call_target, fn_call, array_literal,
              boxed_expr, range, binop_mul, binop_add, unary, varargs_spread, lambda, typecast, lvalue, pointer_op, lexpr, expr,
              ret, var_decl, in_place_binop, assignment, stmt_fn_call,
              comp_op, bin_cond_op, bin_cond, comp, lcond, cond, if_stmt, else_if_stmt, else_stmt, break_stmt, continue_stmt, while_stmt, for_loop_target, for_loop,
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
// TODO: replace all inline block comments w/ the appropriate number of whitespace
// It's important that removing the comments does not change the line/column info!
- (NSString *)preprocessInput:(NSString *)input {
    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    
    lines = [lines lk_mapUsingBlock:^NSString *(NSString *line) {
        return [[self stringByRemovingCommentsInString:line] lk_stringByTrimmingTrailingWhitespace];
    }];
    
    return [lines componentsJoinedByString:@"\n"];
}



- (NSString *)stringByRemovingCommentsInString:(NSString *)line {
    NSInteger length = [line length];
    NSCharacterSet *whitespace = [NSCharacterSet whitespaceCharacterSet];
    
    if (length == 0 || [line lk_allCharactersInCharacterSet:whitespace]) {
        return @"";
    }
    
    
    BOOL onlyWhitespaceSoFar = YES;
    BOOL inStringLiteral = NO;
    BOOL escapeNextCharacter = NO;
    __block NSInteger inlineCommentStartIndex = -1;
    
    __auto_type isParsingInlineComment = ^BOOL() {
        return inlineCommentStartIndex > -1;
    };
    
    for (NSInteger i = 0; i < length; i++) {
        unichar c = [line characterAtIndex:i];
        
        // Check for beginning-of-line comments
        if (onlyWhitespaceSoFar) {
            if (i < length - 3 && c == '/' && [line characterAtIndex:i + 1] == '/') {
                return @"";
            }
            onlyWhitespaceSoFar = [whitespace characterIsMember:c];
        }
        
        // Check for end-of-line comments
        // The only caveat here is excluding slashes in string literals
        
        if (!escapeNextCharacter && !isParsingInlineComment() && c == '"') {
            inStringLiteral = !inStringLiteral;
        }
        
        if (!inStringLiteral && i < length - 1) {
            unichar nextChar = [line characterAtIndex:i + 1];
            if (c == '/') {
                if (nextChar == '/' && !isParsingInlineComment()) {
                    return [line substringToIndex:i];
                } else if (nextChar == '*') {
                    inlineCommentStartIndex = i;
                }
            } else if (c == '*' && isParsingInlineComment() && nextChar == '/') {
                NSRange inlineCommentRange = NSMakeRange(inlineCommentStartIndex, i - inlineCommentStartIndex + 2);
                NSString *replacement = [NSString lk_stringByRepeatingCharacter:' ' count:inlineCommentRange.length];
                line = [line stringByReplacingCharactersInRange:inlineCommentRange withString:replacement];
                
                inlineCommentStartIndex = -1;
                i += 1;
            }
        }
        
        escapeNextCharacter = !escapeNextCharacter && c == '\\';
    }
    
    return line;
}
@end
