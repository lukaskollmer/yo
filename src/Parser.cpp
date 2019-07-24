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
do { if (auto T = currentToken(); T.getKind() != expected) { \
    auto &S = T.getSourceLocation(); \
    std::cout << "[token assert] Expected: " << expected << ", got: " << T.getKind() << ". (file: " << S.filepath << ":" << S.line << ":" << S.column << ")\n";  \
    throw; \
} } while (0)

#define assert_current_token_and_consume(expected) \
do { if (auto T = currentToken(); T.getKind() != expected) { \
    auto &S = T.getSourceLocation(); \
    std::cout << "[token assert] Expected: " << expected << ", got: " << T.getKind() << ". (file: " << S.filepath << ":" << S.line << ":" << S.column << ")\n";  \
    throw; \
} else { consume(); } } while (0)

#define unhandled_token(T)                                                                                                      \
{                                                                                                                               \
    auto &SL = T.getSourceLocation();                                                                                                \
    std::cout << "Unhandled Token: " << T << " at " << SL.filepath << ":" << SL.line << ":" << SL.column << std::endl; throw;   \
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
        for (auto &pair : mapping) {
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



static MappedTokenSet<ast::BinaryOperation::Operation> singleTokenBinopOperatorTokenMapping = {
    { TK::Plus,           BinaryOperation::Operation::Add },
    { TK::Minus,          BinaryOperation::Operation::Sub },
    { TK::Asterisk,       BinaryOperation::Operation::Mul },
    { TK::ForwardSlash,   BinaryOperation::Operation::Div },
    { TK::PercentageSign, BinaryOperation::Operation::Mod },
    { TK::Ampersand,      BinaryOperation::Operation::And },
    { TK::Pipe,           BinaryOperation::Operation::Or  },
    { TK::Circumflex,     BinaryOperation::Operation::Xor }
};




#pragma mark - Parser


#define save_pos(name) auto name = this->position;
#define restore_pos(name) this->position = name;

// How does the parser work?
//
// Position always points to the current token.
// For example, if we parse an identifier, after returning from `ParseIdentifier`, Position would point to the token after that identifier


std::vector<Token> lexFile(std::string &path) {
    return Lexer().lex(util::fs::read_file(path), path);
}


AST Parser::parse(std::string &filepath) {
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
            stmt = parseFunctionDecl(std::make_shared<attributes::FunctionAttributes>(attributeList));
            break;
        }
        case TK::Struct: {
            stmt = parseStructDecl(std::make_shared<attributes::StructAttributes>(attributeList));
            break;
        }
        case TK::Impl:
            stmt = parseImplBlock();
            break;
        case TK::Use:
            resolveImport(); // TODO can/should imports have annotations?
            return parseTopLevelStmt();
        case TK::Using:
            stmt = parseTypealias();
            break;
        default: unhandled_token(currentToken());
    }
    
    stmt->setSourceLocation(startLocation);
    
    return stmt;
}


std::shared_ptr<StructDecl> Parser::parseStructDecl(std::shared_ptr<attributes::StructAttributes> attributes) {
    assert_current_token_and_consume(TK::Struct);
    
    auto decl = std::make_shared<StructDecl>();
    decl->name = parseIdentifier();
    decl->attributes = attributes;
    
    if (currentTokenKind() == TK::LessThanSign) {
        consume();
        while (currentTokenKind() != TK::GreaterSign) {
            decl->templateArguments.push_back(parseIdentifier()->value);
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
    
    auto impl = std::make_shared<ImplBlock>(parseIdentifier()->value);
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    while (currentTokenKind() == TK::Fn || currentTokenKind() == TK::Hashtag) {
        std::vector<attributes::Attribute> attributes;
        if (currentTokenKind() == TK::Hashtag) {
            attributes = parseAttributes();
            LKAssert(currentTokenKind() == TK::Fn);
        }
        auto functionDecl = parseFunctionDecl(std::make_shared<attributes::FunctionAttributes>(attributes));
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
    
    
    while (auto ident = parseIdentifier()) {
        auto key = ident->value;
        
        switch (currentTokenKind()) {
            case TK::OpeningParens: {
                consume();
                std::vector<std::string> members;
                while (auto Ident = parseIdentifier()) {
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
            assert_current_token(TK::Identifier); // Comma must be followed by another attribute
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



std::shared_ptr<FunctionSignature> Parser::parseFunctionSignature(std::shared_ptr<attributes::FunctionAttributes> attributes) {
    auto sourceLoc = getCurrentSourceLocation();
    
    assert_current_token_and_consume(TK::Fn);
    
    auto signature = std::make_shared<ast::FunctionSignature>();
    signature->setSourceLocation(sourceLoc);
    signature->name = parseIdentifier()->value;
    signature->attributes = attributes;
    signature->kind = ast::FunctionSignature::FunctionKind::GlobalFunction; // (for functions in impl blocks, this will be changed to the proper value during irgen preflight)
    
    if (currentTokenKind() == TK::LessThanSign) { // Template function
        signature->isTemplateFunction = true;
        consume();
        while (currentTokenKind() != TK::GreaterSign) {
            signature->templateArgumentNames.push_back(parseIdentifier()->value);
            if (currentTokenKind() == TK::Comma) consume();
        }
        assert_current_token_and_consume(TK::GreaterSign);
    }
    assert_current_token_and_consume(TK::OpeningParens);
    
    //S->Parameters = ParseParameterList(true);
    signature->parameters = {};
    parseFunctionParameterList(signature);
    assert_current_token_and_consume(TK::ClosingParens);
    
    if (currentTokenKind() == TK::Colon) {
        consume();
        signature->returnType = parseType();
    } else {
        signature->returnType = TypeInfo::Void;
    }
    
    return signature;
}


std::shared_ptr<FunctionDecl> Parser::parseFunctionDecl(std::shared_ptr<attributes::FunctionAttributes> attributes) {
    auto decl = std::make_shared<FunctionDecl>();
    decl->signature = parseFunctionSignature(attributes);
    
    if (currentTokenKind() == TK::OpeningCurlyBraces) {
        decl->body = parseComposite();
    } else if (currentTokenKind() == TK::Semicolon) {
        consume();
        decl->body = nullptr;
    }
    
    return decl;
}



std::vector<std::shared_ptr<ast::VariableDecl>> Parser::parseStructPropertyDeclList() {
    std::vector<std::shared_ptr<ast::VariableDecl>> decls;
    
    
    while (currentTokenKind() != TK::ClosingCurlyBraces) {
        // TODO
        //if (CurrentTokenKind() == TK::Hashtag) {
        //    auto attributes = ParseAttributes();
        //}
        auto ident = parseIdentifier();
        assert_current_token_and_consume(TK::Colon);
        auto type = parseType();
        
        auto decl = std::make_shared<ast::VariableDecl>(ident, type);
        decl->setSourceLocation(ident->getSourceLocation());
        decls.push_back(decl);
        
        if (currentTokenKind() == TK::Comma) {
            consume();
            LKAssert(currentTokenKind() != TK::ClosingCurlyBraces);
        }
    }
    
    
    return decls;
}



void Parser::parseFunctionParameterList(std::shared_ptr<ast::FunctionSignature> &signature) {
    constexpr auto delimiter = TK::ClosingParens;
    
    uint64_t index = 0;
    uint64_t pos_lastEntry = UINT64_MAX;
    while (currentTokenKind() != delimiter) {
        LKAssert(pos_lastEntry != position); pos_lastEntry = position;
        
        auto ident = ast::Identifier::emptyIdent();
        TypeInfo *type = TypeInfo::Unresolved;
        
        if (currentTokenKind() == TK::Identifier && peekKind() == TK::Colon) {
            // <ident>: <type>
            ident = parseIdentifier();
            assert_current_token_and_consume(TK::Colon);
            type = parseType();
            if (peekKind(0) == TK::Period && peekKind(1) == TK::Period && peekKind(2) == TK::Period) {
                throw; // TODO implement!
            }
        } else if (currentTokenKind() == TK::Period && peekKind(0) == TK::Period && peekKind(2) == TK::Period) {
            consume(3);
            LKAssert(currentTokenKind() == delimiter);
            signature->attributes->variadic = true;
            return;
        } else {
            ident = std::make_shared<ast::Identifier>(std::string("$").append(std::to_string(index)));
            type = parseType();
        }
        LKAssert(type);
        
        index += 1;
        auto decl = std::make_shared<ast::VariableDecl>(ident, type);
        decl->setSourceLocation(ident->getSourceLocation());
        signature->parameters.push_back(decl);
        
        if (currentTokenKind() == TK::Comma) {
            consume();
            LKAssert(currentTokenKind() != TK::ClosingParens);
        }
    }
}


TypeInfo *Parser::parseType() {
    // TODO add source location to type statements?
    if (currentTokenKind() == TK::Fn) {
        consume();
        TypeInfo::FunctionTypeInfo::CallingConvention cc;
        if (currentTokenKind() == TK::Hashtag) { // TODO use a tilde instead !?
            consume();
            auto cc_ident = parseIdentifier();
            if (cc_ident->value == "c") {
                cc = TypeInfo::FunctionTypeInfo::CallingConvention::C;
            } else if (cc_ident->value == "yo") {
                cc = TypeInfo::FunctionTypeInfo::CallingConvention::Yo;
            } else {
                LKFatalError("unknown calling convention: %s", cc_ident->value.c_str());
            }
        }
        assert_current_token_and_consume(TK::OpeningParens);
        
        std::vector<TypeInfo *> parameterTypes;
        while (auto T = parseType()) {
            parameterTypes.push_back(T);
            if (currentTokenKind() == TK::Comma) {
                consume(); continue;
            } else if (currentTokenKind() == TK::ClosingParens) {
                consume(); break;
            } else {
                unhandled_token(currentToken());
            }
        }
        assert_current_token_and_consume(TK::Colon);
        auto returnType = parseType();
        
        return TypeInfo::makeFunctionType(cc, parameterTypes, returnType);
        
    }
    
    if (currentTokenKind() == TK::Identifier) {
        auto name = parseIdentifier()->value;
        if (currentTokenKind() == TK::LessThanSign) {
            consume();
            std::vector<TypeInfo *> templateParameters;
            while (currentTokenKind() != TK::GreaterSign) {
                auto ty = parseType();
                if (!ty) {
                    LKFatalError("unable to parse type in struct template parameter list");
                }
                templateParameters.push_back(ty);
            }
            assert_current_token_and_consume(TK::GreaterSign);
            return TypeInfo::makeTemplatedStructWithName(name, templateParameters);
        }
        return TypeInfo::getWithName(name);
    }
    
    if (currentTokenKind() == TK::Asterisk) {
        consume();
        return parseType()->getPointerTo();
    }
    
    return nullptr;
}



std::shared_ptr<ast::TypealiasDecl> Parser::parseTypealias() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::Using);
    auto name = parseIdentifier()->value;
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
        if (auto op = parseBinopOperator()) {
            assert_current_token_and_consume(TK::EqualsSign);
            
            auto value = std::make_shared<BinaryOperation>(*op, expr, parseExpression());
            stmt = std::make_shared<Assignment>(expr, value);
            assert_current_token_and_consume(TK::Semicolon);
            stmt->setSourceLocation(sourceLoc);
            return stmt;
        }
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



std::shared_ptr<VariableDecl> Parser::parseVariableDecl() {
    auto sourceLoc = getCurrentSourceLocation();
    assert_current_token_and_consume(TK::Let);
    
    auto identifier = parseIdentifier();
    auto type = TypeInfo::Unresolved;
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
    
    auto decl = std::make_shared<VariableDecl>(identifier, type, initialValue);
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
    auto ident = parseIdentifier();
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



std::shared_ptr<Identifier> Parser::parseIdentifier() {
    if (currentTokenKind() != TK::Identifier) return nullptr;
    auto ident = std::make_shared<Identifier>(currentToken().getData<std::string>());
    ident->setSourceLocation(getCurrentSourceLocation());
    consume();
    return ident;
}

        
std::optional<ast::BinaryOperation::Operation> Parser::parseBinopOperator() {
    auto token = currentTokenKind();
    
    if (!binaryOperatorStartTokens.contains(token)) throw;
    
    if (singleTokenBinopOperatorTokenMapping.contains(token)) {
        consume();
        return singleTokenBinopOperatorTokenMapping[token];
    }
    
    if (token == TK::LessThanSign && peekKind() == TK::LessThanSign) {
        consume(2);
        return BinaryOperation::Operation::Shl;
    }
    
    if (token == TK::GreaterSign && peekKind() == TK::GreaterSign) {
        consume(2);
        return BinaryOperation::Operation::Shr;
    }
    
    return std::nullopt;
}


std::optional<ast::Comparison::Operation> Parser::parseComparisonOperator() {
    using Op = ast::Comparison::Operation;
    
    auto token = currentTokenKind();
    if (!binaryOperatorStartTokens.contains(token)) throw;
    
    auto next = peekKind();
    
    if (token == TK::EqualsSign && next == TK::EqualsSign) {
        consume(2); return Op::EQ;
    }
    
    if (token == TK::ExclamationMark && next == TK::EqualsSign) {
        consume(2); return Op::NE;
    }
    
    if (token == TK::LessThanSign && next == TK::EqualsSign) {
        consume(2); return Op::LE;
    }
    
    if (token == TK::LessThanSign) {
        consume(); return Op::LT;
    }
    
    if (token == TK::GreaterSign && next == TK::EqualsSign) {
        consume(2); return Op::GE;
    }
    
    if (token == TK::GreaterSign) {
        consume(); return Op::GT;
    }
    
    return std::nullopt;
}


std::optional<ast::LogicalOperation::Operation> Parser::parseLogicalOperationOperator() {
    auto token = currentTokenKind();
    if (!binaryOperatorStartTokens.contains(token)) throw;
    
    auto next = peekKind();
    
    if (token == TK::Ampersand && next == TK::Ampersand) {
        consume(2); return ast::LogicalOperation::Operation::And;
    }
    if (token == TK::Pipe && next == TK::Pipe) {
        consume(2); return ast::LogicalOperation::Operation::Or;
    }
    
    return std::nullopt;
}


PrecedenceGroup getOperatorPrecedenceGroup(BinaryOperation::Operation op) {
    switch (op) {
    case BinaryOperation::Operation::Add:
    case BinaryOperation::Operation::Sub:
    case BinaryOperation::Operation::Or:
    case BinaryOperation::Operation::Xor:
        return PrecedenceGroup::Addition;
    case BinaryOperation::Operation::Mul:
    case BinaryOperation::Operation::Div:
    case BinaryOperation::Operation::Mod:
    case BinaryOperation::Operation::And:
        return PrecedenceGroup::Multiplication;
    case BinaryOperation::Operation::Shl:
    case BinaryOperation::Operation::Shr:
        return PrecedenceGroup::Bitshift;
    }
    throw;
}


PrecedenceGroup getOperatorPrecedenceGroup(ast::Comparison::Operation) {
    return PrecedenceGroup::Comparison;
}

PrecedenceGroup getOperatorPrecedenceGroup(ast::LogicalOperation::Operation op) {
    switch (op) {
        case LogicalOperation::Operation::And: return PrecedenceGroup::LogicalConjunction;
        case LogicalOperation::Operation::Or:  return PrecedenceGroup::LogicalDisjunction;
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
        expr = parseIdentifier();
        
        if (currentTokenKind() == TK::Colon && peekKind() == TK::Colon) {
            // a static member reference
            // Q: how do we know that for sure?
            // A: we only end up here if E was null before -> the ident and the two colons are at the beginning of the expression we're parsing
            
            auto typeName = std::dynamic_pointer_cast<ast::Identifier>(expr)->value;
            consume(2);
            auto memberName = parseIdentifier()->value;
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
            consume();
            auto memberName = parseIdentifier()->value;
            expr = std::make_shared<ast::MemberExpr>(expr, memberName);
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
        
        
        
    parse_binop_expr:
        if (binaryOperatorStartTokens.contains(currentTokenKind())) {
            // TODO should thid be a while loop? not really necessary since it's already embedded in a while loop but it might be useful (doesn't have to run all other token comparisons first, before reaching here)?
            save_pos(fallback);
            
            // Since there are multitple binary operators starting with the same initial token (`|` vs `||`, `<` vs `<<`, etc),
            // it's important we parse the different kinds of binary operators in the correct order
            
            if (auto op = parseLogicalOperationOperator()) {
                auto op_precedence = getOperatorPrecedenceGroup(*op);
                if (op_precedence >= precedenceGroupConstraint) {
                    auto rhs = parseExpression(op_precedence);
                    expr = std::make_shared<LogicalOperation>(*op, expr, rhs);
                } else {
                    restore_pos(fallback);
                    return expr;
                }
            } else if (currentTokenKind() == TK::Pipe && peekKind() == TK::GreaterSign) { // `|>` {
                // TODO allow functions w/ explicit template arguments
                // ie: `return argv[1] |> atoi |> static_cast<i64> |> fib;`
                
                if (precedenceGroupConstraint >= PrecedenceGroup::FunctionPipeline) {
                    return expr;
                }
                
                consume(2);
                auto callTarget = parseExpression(PrecedenceGroup::FunctionPipeline);
                expr = std::make_shared<ast::CallExpr>(callTarget, std::vector<std::shared_ptr<ast::Expr>>{ expr });
                continue;
                
            } else if (auto op = parseBinopOperator()) {
                if (currentTokenKind() == TK::EqualsSign) {
                    // <expr> <op>= <expr>;
                    restore_pos(fallback);
                    return expr;
                }
                auto op_precedence = getOperatorPrecedenceGroup(*op);
                if (op_precedence >= precedenceGroupConstraint) {
                    auto rhs = parseExpression(op_precedence);
                    expr = std::make_shared<ast::BinaryOperation>(*op, expr, rhs);
                } else {
                    restore_pos(fallback);
                    return expr;
                }
            } else if (auto op = parseComparisonOperator()) {
                auto op_precedence = getOperatorPrecedenceGroup(*op);
                if (op_precedence >= precedenceGroupConstraint) {
                    auto rhs = parseExpression(PrecedenceGroup::Comparison);
                    expr = std::make_shared<ast::Comparison>(*op, expr, rhs);
                } else {
                    restore_pos(fallback);
                    return expr;
                }
                
            } else {
                // We reach here if the current token is a binary operator starting token, but we didn't manage to parse anything valid out of it
                continue;
            }
        }
        
    } // end ParseExpression main while loop
    
    LKFatalError("should never reach here");
}







std::shared_ptr<ast::CallExpr> Parser::parseCallExpr(std::shared_ptr<ast::Expr> target) {
    std::vector<TypeInfo *> explicitTemplateArgumentTypes;
    
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
    return std::make_shared<ast::CallExpr>(target, callArguments, explicitTemplateArgumentTypes);
}





static TokenSet memberAccessSeparatingTokens = {
    TK::LessThanSign, TK::OpeningParens, TK::OpeningSquareBrackets, TK::Period, TK::Colon
};




std::shared_ptr<ast::MatchExpr> Parser::parseMatchExpr() {
    assert_current_token_and_consume(TK::Match);
    auto target = parseExpression();
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    std::vector<std::shared_ptr<ast::MatchExpr::MatchExprBranch>> branches;
    
    while (true) {
        auto patterns = parseExpressionList(TK::EqualsSign);
        assert_current_token_and_consume(TK::EqualsSign);
        assert_current_token_and_consume(TK::GreaterSign);
        auto expr = parseExpression();
        branches.push_back(std::make_shared<ast::MatchExpr::MatchExprBranch>(patterns, expr));
        
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
    auto &token = currentToken();
    
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
