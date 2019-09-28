//
//  Parser.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Parser.h"

#include <string>
#include <vector>
#include <map>
#include <array>
#include <fstream>
#include <sstream>
#include "Mangling.h"
#include "StdlibResolution.h"

using namespace yo;
using namespace yo::ast;
using namespace yo::parser;

using TK = Token::TokenKind;

#pragma mark - Parser Utils

#define assert_current_token(expected) \
do { if (auto __T = currentToken(); __T.getKind() != (expected)) { \
    const auto& __S = __T.getSourceLocation(); \
    std::cout << "[token assert] Expected: " << (expected) << ", got: " << __T.getKind() << ". (file: " << __S.filepath << ":" << __S.line << ":" << __S.column << ")\n";  \
    throw; \
} } while (0)

#define assert_current_token_and_consume(expected) \
do { if (auto __T = currentToken(); __T.getKind() != (expected)) { \
    const auto& __S = __T.getSourceLocation(); \
    std::cout << "[token assert] Expected: " << (expected) << ", got: " << __T.getKind() << ". (file: " << __S.filepath << ":" << __S.line << ":" << __S.column << ")\n";  \
    throw; \
} else { consume(); } } while (0)

#define unhandled_token(T)                                                                                                      \
{                                                                                                                               \
    const auto& __SL = T.getSourceLocation();                                                                                                \
    std::cout << "Unhandled Token: " << (T) << " at " << __SL.filepath << ":" << __SL.line << ":" << __SL.column << std::endl; throw;   \
}


class TokenSet {
    std::vector<TK> tokens;
    
public:
    TokenSet(std::initializer_list<TK> tokens) : tokens(tokens) {}
    
    bool contains(TK token) {
        return util::vector::contains(tokens, token);
    }
};


template <typename T>
class MappedTokenSet {
    std::map<TK, T> mapping;
    
public:
    MappedTokenSet(std::initializer_list<std::pair<TK, T>> mapping) {
        for (auto& pair : mapping) {
            this->mapping.insert(pair);
        }
    }
    
    bool contains(TK token) {
        return mapping.find(token) != mapping.end();
    }
    
    T &operator [](TK token) {
        return mapping.at(token);
    }
};



#pragma mark - Token Collections

// The initial tokens of all binary operators (binops, comparisons, etc)
static TokenSet binaryOperatorStartTokens = {
    TK::Plus, TK::Minus, TK::Asterisk, TK::ForwardSlash, TK::PercentageSign,
    TK::Ampersand, TK::Pipe, TK::Circumflex, TK::LessThanSign, TK::GreaterSign,
    TK::EqualsSign, TK::ExclamationMark
};



#pragma mark - Parser


#define save_pos(name) auto name = this->position;
#define restore_pos(name) this->position = name;

// How does the parser work?
//
// Position always points to the current token.
// For example, if we parse an identifier, after returning from `ParseIdentifier`, Position would point to the token after that identifier


std::vector<Token> lexFile(const std::string &path) {
    return Lexer().lex(util::fs::read_file(path), path);
}


AST Parser::parse(const std::string &filepath) {
    this->position = 0;
    this->tokens = lexFile(filepath);
    importedFiles.push_back(filepath);
    
    AST ast;
    while (position < tokens.size() && currentTokenKind() != TK::EOF_) {
        ast.push_back(parseTopLevelStmt());
    }
    
    return ast;
}


#include <sys/stat.h>
namespace fs {
    bool file_exists(std::string &path) {
        struct stat s;
        return stat(path.c_str(), &s) == 0;
    }
}


// TODO move the entire module resolution stuff somewhere else !?
std::string Parser::resolveImportPathRelativeToBaseDirectory(const std::string &moduleName, const std::string &baseDirectory) {
    if (moduleName[0] == '/') { // absolute path
        return moduleName;
    }
    
    std::string path = std::string(baseDirectory).append("/").append(moduleName).append(".yo");
    if (fs::file_exists(path)) return path;
    
    LKFatalError("Unable to resolve import of '%s' relative to '%s'", moduleName.c_str(), baseDirectory.c_str());
}



void Parser::resolveImport() {
    auto baseDirectory = util::string::excludingLastPathComponent(currentToken().getSourceLocation().filepath);
    assert_current_token_and_consume(TK::Use);
    
    auto moduleName = parseStringLiteral()->value;
    assert_current_token_and_consume(TK::Semicolon);
    
    std::vector<Token> newTokens;
    
    auto isStdlibImport = moduleName[0] == ':';
    if (isStdlibImport && util::vector::contains(importedFiles, moduleName)) {
        return;
    } else if (isStdlibImport && !useCustomStdlibRoot) {
        importedFiles.push_back(moduleName);
        newTokens = Lexer().lex(stdlib_resolution::getContentsOfModuleWithName(moduleName), moduleName);
    } else {
        if (isStdlibImport) {
            importedFiles.push_back(moduleName);
            moduleName.erase(moduleName.begin());
            baseDirectory = customStdlibRoot;
        }
        auto path = resolveImportPathRelativeToBaseDirectory(moduleName, baseDirectory);
        if (util::vector::contains(importedFiles, path)) return;
        importedFiles.push_back(path);
        newTokens = lexFile(path);
    }
    
    tokens.insert(tokens.begin() + position,
                  newTokens.begin(),
                  newTokens.end() - 1); // exclude EOF_
}




std::shared_ptr<TopLevelStmt> Parser::parseTopLevelStmt() {
    std::shared_ptr<TopLevelStmt> stmt;
    auto attributeList = parseAttributes();
    auto startLocation = currentToken().getSourceLocation();
    
    switch (currentToken().getKind()) {
        case TK::Fn: {
            stmt = parseFunctionDecl(attributes::FunctionAttributes(attributeList));
            break;
        }
        case TK::Struct: {
            stmt = parseStructDecl(attributes::StructAttributes(attributeList));
            break;
        }
        case TK::Impl: {
            stmt = parseImplBlock();
            break;
        }
        case TK::Use: {
            if (peekKind() == TK::StringLiteral) {
                resolveImport();
                return parseTopLevelStmt();
            } else if (peekKind() == TK::Ident) {
                stmt = parseTypealias();
                break;
            }
        }
        default: unhandled_token(currentToken());
    }
    
    stmt->setSourceLocation(startLocation);
    
    return stmt;
}


std::shared_ptr<StructDecl> Parser::parseStructDecl(attributes::StructAttributes attributes) {
    assert_current_token_and_consume(TK::Struct);
    
    auto decl = std::make_shared<StructDecl>();
    decl->name = parseIdentAsString();
    decl->attributes = attributes;
    
    if (currentTokenKind() == TK::LessThanSign) {
        consume();
        while (currentTokenKind() != TK::GreaterSign) {
            decl->templateArguments.push_back(parseIdentAsString());
            if (currentTokenKind() == TK::Comma) consume();
        }
        assert_current_token_and_consume(TK::GreaterSign);
    }
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    decl->members = parseStructPropertyDeclList();
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return decl;
}


std::shared_ptr<ImplBlock> Parser::parseImplBlock() {
    assert_current_token_and_consume(TK::Impl);
    
    auto impl = std::make_shared<ImplBlock>(parseIdentAsString());
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    while (currentTokenKind() == TK::Fn || currentTokenKind() == TK::Hashtag) {
        std::vector<attributes::Attribute> attributes;
        if (currentTokenKind() == TK::Hashtag) {
            attributes = parseAttributes();
            LKAssert(currentTokenKind() == TK::Fn);
        }
        auto functionDecl = parseFunctionDecl(attributes::FunctionAttributes(attributes));
        impl->methods.push_back(functionDecl);
    }
    
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return impl;
}


std::vector<yo::attributes::Attribute> Parser::parseAttributes() {
    // TODO save attribute source locations!
    if (currentTokenKind() != TK::Hashtag) return {};
    consume();
    assert_current_token_and_consume(TK::OpeningSquareBrackets);
    
    std::vector<yo::attributes::Attribute> attributes;
    
    
    while (auto ident = parseIdent()) {
        auto key = ident->value;
        
        switch (currentTokenKind()) {
            case TK::OpeningParens: {
                consume();
                std::vector<std::string> members;
                while (auto Ident = parseIdent()) {
                    members.push_back(ident->value);
                    if (currentTokenKind() == TK::Comma) {
                        consume();
                    } else if (currentTokenKind() == TK::ClosingParens) {
                        break;
                    } else {
                        unhandled_token(currentToken());
                    }
                }
                assert_current_token_and_consume(TK::ClosingParens);
                attributes.push_back(yo::attributes::Attribute(key, members));
                break;
            }
            case TK::EqualsSign: {
                consume();
                if (auto value = parseStringLiteral()) {
                    LKAssert(value->kind == ast::StringLiteral::StringLiteralKind::NormalString);
                    attributes.push_back(yo::attributes::Attribute(key, value->value));
                } else if (auto ident = parseIdent()) {
                    attributes.push_back(yo::attributes::Attribute(key, ident->value));
                } else {
                    LKFatalError("unable to parse attribute value");
                }
                break;
            }
            
            case TK::Comma:
            case TK::ClosingSquareBrackets:
                attributes.push_back(yo::attributes::Attribute(key));
                break;
                
            default:
                unhandled_token(currentToken());
        }

        if (currentTokenKind() == TK::Comma) {
            consume();
            assert_current_token(TK::Ident); // Comma must be followed by another attribute
            continue;
        } else if (currentTokenKind() == TK::ClosingSquareBrackets) {
            consume();
            if (currentTokenKind() == TK::Hashtag && peekKind() == TK::OpeningSquareBrackets) {
                consume(2);
                continue;
            } else {
                break;
            }
        }
    }
    
    return attributes;
}



std::shared_ptr<FunctionDecl> Parser::parseFunctionDecl(attributes::FunctionAttributes attributes) {
    auto functionKind = FunctionKind::GlobalFunction; // initial assumption
    
    assert_current_token(TK::Fn);
    const auto& loc = getCurrentSourceLocation();
    consume();
    
    FunctionSignature signature;
    
    auto name = parseIdentAsString();
    if (name == "operator") {
        auto op = parseOperator();
        LKAssert(op.has_value());
        functionKind = FunctionKind::OperatorOverload;
        name = mangling::encodeOperator(op.value());
    }
    
    
    if (currentTokenKind() == TK::LessThanSign) { // template list
        consume();
        while (currentTokenKind() != TK::GreaterSign) {
            signature.templateArgumentNames.push_back(parseIdentAsString());
            if (currentTokenKind() == TK::Comma) {
                consume();
            } else {
                assert_current_token_and_consume(TK::GreaterSign);
                break;
            }
        }
        LKAssert(!signature.templateArgumentNames.empty());
    }
    
    
    assert_current_token_and_consume(TK::OpeningParens);
    
    parseFunctionParameterList(signature, attributes);
    assert_current_token_and_consume(TK::ClosingParens);
    
    if (currentTokenKind() == TK::Minus && peekKind() == TK::GreaterSign) {
        consume(2);
        signature.returnType = parseType();
    } else {
        signature.returnType = TypeDesc::makeNominal("void");
    }
    
    auto fnDecl = std::make_shared<FunctionDecl>(functionKind, name, signature, attributes);
    fnDecl->setSourceLocation(loc);
    
    if (currentTokenKind() == TK::Semicolon) {
        // forward declaration
        consume();
        return fnDecl;
    }
    
    fnDecl->setBody(parseComposite()->statements);
    return fnDecl;
}










std::vector<std::shared_ptr<ast::VarDecl>> Parser::parseStructPropertyDeclList() {
    std::vector<std::shared_ptr<ast::VarDecl>> decls;
    
    while (currentTokenKind() != TK::ClosingCurlyBraces) {
        // TODO
        //if (CurrentTokenKind() == TK::Hashtag) {
        //    auto attributes = ParseAttributes();
        //}
        auto loc = getCurrentSourceLocation();
        auto ident = parseIdentAsString();
        assert_current_token_and_consume(TK::Colon);
        auto type = parseType();
        
        auto decl = std::make_shared<ast::VarDecl>(ident, type);
        decl->setSourceLocation(loc);
        decls.push_back(decl);
        
        if (currentTokenKind() == TK::Comma) {
            consume();
            LKAssert(currentTokenKind() != TK::ClosingCurlyBraces);
        }
    }
    
    
    return decls;
}



// TODO why is this a separate function? re-inline!
void Parser::parseFunctionParameterList(FunctionSignature& signature, attributes::FunctionAttributes& attributes) {
    constexpr auto delimiter = TK::ClosingParens;
    
    uint64_t index = 0;
    uint64_t pos_lastEntry = UINT64_MAX;
    while (currentTokenKind() != delimiter) {
        LKAssert(pos_lastEntry != position); pos_lastEntry = position;
        
        auto loc = getCurrentSourceLocation();
        std::string ident;
        std::shared_ptr<TypeDesc> type;
        
        if (currentTokenKind() == TK::Ident && peekKind() == TK::Colon) {
            // <ident>: <type>
            ident = parseIdentAsString();
            assert_current_token_and_consume(TK::Colon);
            type = parseType();
            if (peekKind(0) == TK::Period && peekKind(1) == TK::Period && peekKind(2) == TK::Period) {
                throw; // TODO implement!
            }
        } else if (currentTokenKind() == TK::Period && peekKind(0) == TK::Period && peekKind(2) == TK::Period) {
            consume(3);
            LKAssert(currentTokenKind() == delimiter);
            attributes.variadic = true;
            return;
        } else {
            ident = std::string("$").append(std::to_string(index));
            type = parseType();
        }
        LKAssert(type);
        
        index += 1;
        auto decl = std::make_shared<ast::VarDecl>(ident, type);
        decl->setSourceLocation(loc);
        signature.parameters.push_back(decl);
        
        if (currentTokenKind() == TK::Comma) {
            consume();
            LKAssert(currentTokenKind() != TK::ClosingParens);
        }
    }
}



// Attempts to extract a calling convention from a type's attributes list
// If no calling convention is explicitly specified, the default calling convention is returned
yo::irgen::CallingConvention extractCallingConventionAttribute(const std::vector<attributes::Attribute> &attributes) {
    using CC = yo::irgen::CallingConvention;
    
    if (auto attr = util::vector::first_where(attributes, [](auto& attr) { return attr.getKey() == "convention"; })) {
        auto value = attr.value().getData<std::string>();
        if (value == "C") {
            return CC::C;
        } else {
            LKFatalError("unknown calling convention '%s'", value.c_str());
        }
    }
    
    return CC::C; // TODO change default calling convention / get it from somewhere to that it's the same everywhere?
}


// Parse a type description
//
// This can be:
// - A simple nominal type: `i64`, `String`, ...
// - A pointer
// - A function type
// - A structural type (TODO!?)
std::shared_ptr<TypeDesc> Parser::parseType() {
    // TODO add source location to type statements?
    
    auto attributes = parseAttributes();
    
    switch (currentTokenKind()) {
        case TK::Ampersand: {
            const auto &loc = getCurrentSourceLocation();
            consume();
            return TypeDesc::makeReference(parseType(), loc);
        }
        case TK::Asterisk: {
            auto loc = getCurrentSourceLocation();
            consume();
            return TypeDesc::makePointer(parseType(), loc);
        }
        case TK::Ident: {
            // Issue: we have to be careful here. consider the following code:
            // `x < static_cast<T>(0)`. until we reach the opening parens, the expression is ambiguous.
            // Solution to this is never allowing parens in type declarations?
            // - `x < y<T>()` -> expr
            // - `x<y>::z()` -> expr
            
            save_pos(fallback);
            
            auto loc = getCurrentSourceLocation();
            auto name = parseIdentAsString();
            if (currentTokenKind() == TK::Colon) {
                LKFatalError("TODO implement?");
            
            } else if (currentTokenKind() == TK::LessThanSign) {
                consume();
                std::vector<std::shared_ptr<TypeDesc>> paramTys;
                while (auto ty = parseType()) {
                    paramTys.push_back(ty);
                    if (currentTokenKind() == TK::Comma) {
                        consume();
                        continue;
                    } else if (currentTokenKind() == TK::GreaterSign) {
                        break;
                    } else {
                        unhandled_token(currentToken());
                    }
                }
                assert_current_token_and_consume(TK::GreaterSign);
                LKAssert(!paramTys.empty());
                if (currentTokenKind() == TK::OpeningParens) {
                    // ie `if x < static_cast<T>(0)`.
                    restore_pos(fallback);
                    return nullptr;
                }
                unhandled_token(currentToken());
                LKFatalError("TODO implement!");
            }
            
            return TypeDesc::makeNominal(name, loc);
        }
        case TK::OpeningParens: {
            auto loc = getCurrentSourceLocation();
            consume();
            std::vector<std::shared_ptr<TypeDesc>> types;
            while (currentTokenKind() != TK::ClosingParens) {
                types.push_back(parseType());
                if (currentTokenKind() == TK::Comma) {
                    consume();
                }
            }
            assert_current_token_and_consume(TK::ClosingParens);
            
            if (currentTokenKind() == TK::Minus && peekKind() == TK::GreaterSign) {
                // Function type
                consume(2);
                auto cc = extractCallingConventionAttribute(attributes);
                auto returnType = parseType();
                return TypeDesc::makeFunction(cc, returnType, types, loc);
            } else {
                LKFatalError("tuple");
            }
        }
        default: return nullptr;
    }
}



std::shared_ptr<ast::TypealiasDecl> Parser::parseTypealias() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::Use);
    auto name = parseIdentAsString();
    assert_current_token_and_consume(TK::EqualsSign);
    auto type = parseType();
    assert_current_token_and_consume(TK::Semicolon);
    auto decl = std::make_shared<ast::TypealiasDecl>(name, type);
    decl->setSourceLocation(sourceLoc);
    return decl;
}



#pragma mark - Local Statements



std::shared_ptr<Composite> Parser::parseComposite() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    auto stmt = std::make_shared<Composite>();
    stmt->setSourceLocation(sourceLoc);
    while (currentTokenKind() != TK::ClosingCurlyBraces) {
        stmt->statements.push_back(parseLocalStmt());
    }
    
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return stmt;
}






std::shared_ptr<LocalStmt> Parser::parseLocalStmt() {
    // TODO make this a switch?
    if (currentTokenKind() == TK::Return) {
        return parseReturnStmt();
    }
    
    if (currentTokenKind() == TK::Let) {
        return parseVariableDecl();
    }
    
    if (currentTokenKind() == TK::If) {
        return parseIfStmt();
    }
    
    if (currentTokenKind() == TK::While) {
        return parseWhileStmt();
    }
    
    if (currentTokenKind() == TK::For) {
        return parseForLoop();
    }
    
    auto sourceLoc = getCurrentSourceLocation();
    
    std::shared_ptr<LocalStmt> stmt;
    std::shared_ptr<Expr> expr; // A partially-parsed part of a local statement
    
    expr = parseExpression();
    expr->setSourceLocation(sourceLoc);
    
    if (currentTokenKind() == TK::EqualsSign) { // Assignment
        consume();
        auto value = parseExpression();
        assert_current_token_and_consume(TK::Semicolon);
        auto assignment = std::make_shared<ast::Assignment>(expr, value);
        assignment->setSourceLocation(sourceLoc);
        return assignment;
    }
    
    if (binaryOperatorStartTokens.contains(currentTokenKind())) {
        LKFatalError("TODO reimplement");
//        if (auto op = parseBinopOperator()) {
//            assert_current_token_and_consume(TK::EqualsSign);
//
//            auto value = std::make_shared<BinOp>(*op, expr, parseExpression());
//            stmt = std::make_shared<Assignment>(expr, value);
//            assert_current_token_and_consume(TK::Semicolon);
//            stmt->setSourceLocation(sourceLoc);
//            return stmt;
//        }
    }
    
    if (currentTokenKind() == TK::Semicolon) {
        consume();
        if (expr) {
            auto exprStmt = std::make_shared<ast::ExprStmt>(expr);
            exprStmt->setSourceLocation(sourceLoc);
            return exprStmt;
        }
    }
    
    unhandled_token(currentToken())
}



std::shared_ptr<ReturnStmt> Parser::parseReturnStmt() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::Return);
    
    if (currentTokenKind() == TK::Semicolon) {
        consume();
        return std::make_shared<ReturnStmt>(nullptr);
    }
    
    auto expr = parseExpression();
    assert_current_token_and_consume(TK::Semicolon);
    auto retStmt = std::make_shared<ReturnStmt>(expr);
    retStmt->setSourceLocation(sourceLoc);
    return retStmt;
}



std::shared_ptr<VarDecl> Parser::parseVariableDecl() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::Let);
    
    auto ident = parseIdentAsString();
    std::shared_ptr<TypeDesc> type;
    std::shared_ptr<Expr> initialValue;
    
    if (currentTokenKind() == TK::Colon) {
        consume();
        type = parseType();
    }
    
    if (currentTokenKind() == TK::EqualsSign) {
        consume();
        initialValue = parseExpression();
    }
    
    assert_current_token_and_consume(TK::Semicolon);
    
    auto decl = std::make_shared<VarDecl>(ident, type, initialValue);
    decl->setSourceLocation(sourceLoc);
    return decl;
}



std::shared_ptr<IfStmt> Parser::parseIfStmt() {
    using Kind = ast::IfStmt::Branch::BranchKind;
    
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::If);
    
    std::vector<std::shared_ptr<IfStmt::Branch>> branches;
    
    auto mainExpr = parseExpression();
    assert_current_token(TK::OpeningCurlyBraces);
    
    branches.push_back(std::make_shared<IfStmt::Branch>(Kind::If,
                                                        mainExpr,
                                                        parseComposite()));
    
    while (currentTokenKind() == TK::Else && peekKind() == TK::If) {
        consume(2);
        auto expr = parseExpression();
        assert_current_token(TK::OpeningCurlyBraces);
        auto body = parseComposite();
        branches.push_back(std::make_shared<IfStmt::Branch>(Kind::ElseIf, expr, body));
    }
    
    if (currentTokenKind() == TK::Else && peekKind() == TK::OpeningCurlyBraces) {
        consume();
        branches.push_back(std::make_shared<IfStmt::Branch>(Kind::Else, nullptr, parseComposite()));
    }
    
    auto ifStmt = std::make_shared<ast::IfStmt>(branches);
    ifStmt->setSourceLocation(sourceLoc);
    return ifStmt;
}




std::shared_ptr<ast::WhileStmt> Parser::parseWhileStmt() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::While);
    
    auto condition = parseExpression();
    assert_current_token(TK::OpeningCurlyBraces);
    
    auto stmt = std::make_shared<ast::WhileStmt>(condition, parseComposite());
    stmt->setSourceLocation(sourceLoc);
    return stmt;
}



std::shared_ptr<ForLoop> Parser::parseForLoop() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::For);
    auto ident = parseIdent();
    assert_current_token_and_consume(TK::In);
    auto expr = parseExpression();
    assert_current_token(TK::OpeningCurlyBraces);
    auto body = parseComposite();
    auto stmt = std::make_shared<ForLoop>(ident, expr, body);
    stmt->setSourceLocation(sourceLoc);
    return stmt;
}



#pragma mark - Expressions



// Parses a (potentially empty) list of expressions separated by commas, until Delimiter is reached
// The delimiter is not consumed
std::vector<std::shared_ptr<Expr>> Parser::parseExpressionList(Token::TokenKind delimiter) {
    if (currentTokenKind() == delimiter) return {};
    
    std::vector<std::shared_ptr<Expr>> expressions;
    
    do {
        expressions.push_back(parseExpression());
        LKAssert(currentTokenKind() == TK::Comma || currentTokenKind() == delimiter);
        if (currentTokenKind() == TK::Comma) consume();
    } while (currentTokenKind() != delimiter);
    
    assert_current_token(delimiter);
    return expressions;
}


std::string Parser::parseIdentAsString() {
    assert_current_token(TK::Ident);
    auto val = currentToken().getData<std::string>();
    consume();
    return val;
}

std::shared_ptr<Ident> Parser::parseIdent() {
    if (currentTokenKind() != TK::Ident) return nullptr;
    auto ident = std::make_shared<Ident>(currentToken().getData<std::string>());
    ident->setSourceLocation(getCurrentSourceLocation());
    consume();
    return ident;
}




PrecedenceGroup getOperatorPrecedenceGroup(Operator op) {
    switch (op) {
        case Operator::Add:
        case Operator::Sub:
        case Operator::Or:
        case Operator::Xor:
            return PrecedenceGroup::Addition;
        
        case Operator::Mul:
        case Operator::Div:
        case Operator::Mod:
        case Operator::And:
            return PrecedenceGroup::Multiplication;
        
        case Operator::Shl:
        case Operator::Shr:
            return PrecedenceGroup::Bitshift;
        
        case Operator::Neg:
        case Operator::BNot:
        case Operator::BNeg:
            return PrecedenceGroup::PrefixOperator;
        
        case Operator::LAnd:
            return PrecedenceGroup::LogicalConjunction;
        
        case Operator::LOr:
            return PrecedenceGroup::LogicalDisjunction;
        
        case Operator::EQ:
        case Operator::NE:
        case Operator::LT:
        case Operator::LE:
        case Operator::GT:
        case Operator::GE:
            return PrecedenceGroup::Comparison;
        
        case Operator::FnPipe:
            return PrecedenceGroup::FunctionPipeline;
        
        case Operator::Assign:
            return PrecedenceGroup::Assignment;
    }
}





// Tokens that, if they appear on their own, mark the end of an expression
static TokenSet expressionDelimitingTokens = {
    TK::ClosingParens, TK::Semicolon, TK::Comma, TK::OpeningCurlyBraces, TK::ClosingSquareBrackets, TK::EqualsSign, TK::ClosingCurlyBraces
};


std::shared_ptr<Expr> Parser::parseExpression(PrecedenceGroup precedenceGroupConstraint) {
    if (expressionDelimitingTokens.contains(currentTokenKind())) {
        return nullptr;
    }
    
    auto sourceLoc = getCurrentSourceLocation();
    
    std::shared_ptr<Expr> expr;
    
    if (currentTokenKind() == TK::OpeningParens) {
        consume();
        expr = parseExpression();
        assert_current_token_and_consume(TK::ClosingParens);
    
    }
    
    if (!expr && currentTokenKind() == TK::Match) {
        expr = parseMatchExpr();
    }
    
    if (!expr) {
        expr = parseNumberLiteral();
    }
    
    if (!expr) {
        expr = parseUnaryExpr();
    }
    
    if (!expr) {
        expr = parseStringLiteral();
    }
    
    if (!expr) {
        expr = parseIdent();
        
        if (currentTokenKind() == TK::Colon && peekKind() == TK::Colon) {
            // a static member reference
            // Q: how do we know that for sure?
            // A: we only end up here if E was null before -> the ident and the two colons are at the beginning of the expression we're parsing
            
            auto typeName = std::dynamic_pointer_cast<ast::Ident>(expr)->value;
            consume(2);
            auto memberName = parseIdentAsString();
            expr = std::make_shared<ast::StaticDeclRefExpr>(typeName, memberName);
        }
    }
    
    expr->setSourceLocation(sourceLoc);
    
    
    ast::Expr *_last_entry_expr_ptr = nullptr;
    
    while (true) {
        LKAssert(expr);
        if (_last_entry_expr_ptr == expr.get()) {
            unhandled_token(currentToken());
        }
        _last_entry_expr_ptr = expr.get();
        
        if (expressionDelimitingTokens.contains(currentTokenKind())) {
            if (currentTokenKind() == TK::EqualsSign && peekKind() == TK::EqualsSign) {
                goto parse_binop_expr;
            }
            return expr;
        }
        
        // Q: What's going on here?
        // A: Basically, the idea is to catch all tokens that might indicate what kind of expression this is (call, member, binop, comparison, etc)
        //    in this big while loop
        // Q: What kinds of expressions are handled in here?
        // A: Everything that isn't the "initial" part of an expression (that's why E has to be nunnull on entry)
        // Q: How does this work, being a loop?
        // A: We return from the loop when encountering something that doesn't belong to the current precedence group
        // The long term plan is splitting this up in a bunch of functions and having ParseExpression call them from the while loop, depending on which kind of expression seems most likely based on the current token
        
//    parse_call_expr:
        if (currentTokenKind() == TK::LessThanSign || currentTokenKind() == TK::OpeningParens) {
            if (auto callExpr = parseCallExpr(expr)) {
                expr = callExpr;
                continue; // TODO is the consume actually required/good?
            } else if (currentTokenKind() == TK::LessThanSign) {
                goto parse_binop_expr; // use this as a hint that this is probably a binop expression?
            }
        }
        
        
        
//    parse_member_expr:
        if (currentTokenKind() == TK::Period) { // member expr
            const auto& loc = getCurrentSourceLocation();
            consume();
            auto memberName = parseIdentAsString();
            expr = std::make_shared<ast::MemberExpr>(expr, memberName);
            expr->setSourceLocation(loc);
            // TODO goto parse_call_expr if the current token is `<` or `(`?
            // member expressions are probably somewhat often also call targets?
        }
        
        
        
//    parse_subscript_expr:
        if (currentTokenKind() == TK::OpeningSquareBrackets) {
            consume();
            auto offsetExpr = parseExpression();
            assert_current_token_and_consume(TK::ClosingSquareBrackets);
            expr = std::make_shared<ast::SubscriptExpr>(expr, offsetExpr);
        }
        
        
    parse_binop_expr: {
        save_pos(fallback);
        
        const auto& operatorLoc = getCurrentSourceLocation();
        if (auto op_ = parseOperator()) {
            auto op = op_.value();
            
            if (op == Operator::FnPipe) {
                if (precedenceGroupConstraint >= PrecedenceGroup::FunctionPipeline) {
                    restore_pos(fallback);
                    return expr;
                }
                
                //consume(2);
                auto callTarget = parseExpression(PrecedenceGroup::FunctionPipeline);
                expr = std::make_shared<ast::CallExpr>(callTarget, std::vector<std::shared_ptr<ast::Expr>>{ expr });
                continue;
            }
            
            
            auto precedence = getOperatorPrecedenceGroup(op);
            
            if (precedence >= precedenceGroupConstraint) {
                auto rhs = parseExpression(precedence);
                expr = std::make_shared<ast::BinOp>(op, expr, rhs);
                expr->setSourceLocation(operatorLoc);
            } else {
                restore_pos(fallback);
                return expr;
            }
        }
    } // end of parse_binop_expr
    } // end ParseExpression main while loop
    
    LKFatalError("should never reach here");
}





std::optional<ast::Operator> Parser::parseOperator() {
    switch (currentTokenKind()) {
        case TK::Plus:
            consume();
            return Operator::Add;
        
        case TK::Minus:
            consume();
            return Operator::Sub;
        
        case TK::Asterisk:
            consume();
            return Operator::Mul;
        
        case TK::ForwardSlash:
            consume();
            return Operator::Div;
        
        case TK::PercentageSign:
            consume();
            return Operator::Mod;
        
        case TK::Circumflex:
            consume();
            return Operator::Xor;
            
        case TK::Ampersand: {
            if (peekKind() == TK::Ampersand) {
                consume(2);
                return Operator::LAnd;
            } else {
                consume();
                return Operator::And;
            }
        }
        
        case TK::LessThanSign: {
            if (peekKind() == TK::LessThanSign && peekKind(2) != TK::Ident) {
                consume(2);
                return Operator::Shl;
            } else if (peekKind() == TK::EqualsSign) {
                consume(2);
                return Operator::LE;
            } else {
                consume();
                return Operator::LT;
            }
        }
        
        case TK::GreaterSign: {
            if (peekKind() == TK::GreaterSign) {
                consume(2);
                return Operator::Shr;
            } else if (peekKind() == TK::EqualsSign) {
                consume(2);
                return Operator::GE;
            } else {
                consume();
                return Operator::GT;
            }
        }
        
        case TK::Tilde:
            consume();
            return Operator::BNot;
        
        case TK::ExclamationMark: {
            if (peekKind() == TK::EqualsSign) {
                consume(2);
                return Operator::NE;
            } else {
                consume();
                return Operator::BNot;
            }
        }
        
        case TK::EqualsSign: {
            if (peekKind() == TK::EqualsSign) {
                consume(2);
                return Operator::EQ;
            } else {
                consume();
                return Operator::Assign;
            }
        }
        
        case TK::Pipe: {
            if (peekKind() == TK::GreaterSign) {
                consume(2);
                return Operator::FnPipe;
            } else if (peekKind() == TK::Pipe) {
                consume(2);
                return Operator::LOr;
            } else {
                consume();
                return Operator::Or;
            }
        }
        default:
            return std::nullopt;
    }
}




std::shared_ptr<ast::CallExpr> Parser::parseCallExpr(std::shared_ptr<ast::Expr> target) {
    std::vector<std::shared_ptr<TypeDesc>> explicitTemplateArgumentTypes;
    
    if (currentTokenKind() == TK::LessThanSign) {
        save_pos(pos_of_less_than_sign);
        consume();
        while (currentTokenKind() != TK::GreaterSign) { // TODO this might become a problem if there is an `<>` operator?
            auto type = parseType();
            if (!type) {
                restore_pos(pos_of_less_than_sign);
                return nullptr;
            }
            explicitTemplateArgumentTypes.push_back(type);
            
            if (currentTokenKind() == TK::Comma) {
                consume(); continue;
            } else if (currentTokenKind() == TK::GreaterSign) {
                break;
            } else {
                // If we end up here, the less than sign is probably part of a comparison or bit shift
                restore_pos(pos_of_less_than_sign);
                return nullptr;
            }
        }
        assert_current_token_and_consume(TK::GreaterSign);
        LKAssert(!explicitTemplateArgumentTypes.empty()); // TODO allow empty explicit template lists?
    }
    assert_current_token_and_consume(TK::OpeningParens);
    
    auto callArguments = parseExpressionList(TK::ClosingParens);
    assert_current_token_and_consume(TK::ClosingParens);
    auto callExpr = std::make_shared<ast::CallExpr>(target, callArguments, explicitTemplateArgumentTypes);
    callExpr->setSourceLocation(target->getSourceLocation());
    return callExpr;
}





static TokenSet memberAccessSeparatingTokens = {
    TK::LessThanSign, TK::OpeningParens, TK::OpeningSquareBrackets, TK::Period, TK::Colon
};




std::shared_ptr<ast::MatchExpr> Parser::parseMatchExpr() {
    assert_current_token_and_consume(TK::Match);
    auto target = parseExpression();
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    std::vector<MatchExpr::MatchExprBranch> branches;
    
    while (true) {
        auto patterns = parseExpressionList(TK::EqualsSign);
        assert_current_token_and_consume(TK::EqualsSign);
        assert_current_token_and_consume(TK::GreaterSign);
        auto expr = parseExpression();
        branches.push_back(MatchExpr::MatchExprBranch(patterns, expr));
        
        switch (currentTokenKind()) {
            case TK::Comma:
                consume();
                continue;
            case TK::ClosingCurlyBraces:
                goto ret;
            default:
                unhandled_token(currentToken());
        }
    }
ret:
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return std::make_shared<ast::MatchExpr>(target, branches);
}





// MARK: Literals


std::shared_ptr<NumberLiteral> Parser::parseNumberLiteral() {
    uint64_t value;
    NumberLiteral::NumberType type;
    bool isNegated = false;
    
    save_pos(prev_pos)
    
    if (currentTokenKind() == TK::Minus) {
        consume();
        isNegated = true;
    }
    
    switch (currentTokenKind()) {
        case TK::IntegerLiteral:
            value = currentToken().getData<uint64_t>();
            type = NumberLiteral::NumberType::Integer;
            break;
        case TK::DoubleLiteral: throw;
        case TK::CharLiteral:
            value = currentToken().getData<char>();
            type = NumberLiteral::NumberType::Character;
            break;
        case TK::BoolLiteral:
            value = currentToken().getData<bool>();
            type = NumberLiteral::NumberType::Boolean;
            break;
        default:
            restore_pos(prev_pos);
            return nullptr;
    }
    consume();
    
    if (isNegated) {
        value *= -1;
    }
    return std::make_shared<NumberLiteral>(value, type);
}




std::shared_ptr<StringLiteral> Parser::parseStringLiteral() {
    const auto& token = currentToken();
    
    if (token.getKind() != TK::StringLiteral && token.getKind() != TK::ByteStringLiteral) {
        return nullptr;
    }
    
    auto value = token.getData<std::string>();
    auto kind = token.getKind() == TK::StringLiteral
        ? StringLiteral::StringLiteralKind::NormalString
        : StringLiteral::StringLiteralKind::ByteString;
    
    consume();
    return std::make_shared<StringLiteral>(value, kind);
}


static MappedTokenSet<ast::UnaryExpr::Operation> unaryOperators = {
    { TK::Minus, UnaryExpr::Operation::Negate },
    { TK::Tilde, UnaryExpr::Operation::BitwiseNot },
    { TK::ExclamationMark, UnaryExpr::Operation::LogicalNegation}
};

std::shared_ptr<UnaryExpr> Parser::parseUnaryExpr() {
    if (!unaryOperators.contains(currentTokenKind())) return nullptr;
    auto op = unaryOperators[currentTokenKind()];
    consume();
    auto expr = parseExpression(PrecedenceGroup::PrefixOperator);
    return std::make_shared<UnaryExpr>(op, expr);
}
