//
//  Parser.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

// NOTE: Most (all?) of this is crap and will be rewritten eventually, once i know how parsers are actually supposed to work (ideally some time after passing theo and/or cc?)

#include "Parser.h"

#include <string>
#include <vector>
#include <map>
#include <array>
#include <fstream>
#include <sstream>
#include "Mangling.h"
#include "StdlibResolution.h"

using namespace ast;

using TK = Token::TokenKind;

#pragma mark - Parser Utils

#define assert_current_token(Expected) \
do { if (auto T = CurrentToken(); T.Kind != Expected) { \
    auto &S = T.SourceLocation; \
    std::cout << "[token assert] Expected: " << Expected << ", got: " << T.Kind << ". (file: " << S.Filename << ":" << S.Line << ":" << S.Column << ")\n";  \
    throw; \
} } while (0)

#define assert_current_token_and_consume(Expected) \
do { if (auto T = CurrentToken(); T.Kind != Expected) { \
    auto &S = T.SourceLocation; \
    std::cout << "[token assert] Expected: " << Expected << ", got: " << T.Kind << ". (file: " << S.Filename << ":" << S.Line << ":" << S.Column << ")\n";  \
    throw; \
} else { Consume(); } } while (0)

#define unhandled_token(T)                                                                                                      \
{                                                                                                                               \
    auto &SL = T.SourceLocation;                                                                                                \
    std::cout << "Unhandled Token: " << T << " at " << SL.Filename << ":" << SL.Line << ":" << SL.Column << std::endl; throw;   \
}


class TokenSet {
    std::vector<TK> Tokens;
    
public:
    TokenSet(std::initializer_list<TK> Tokens) : Tokens(Tokens) {}
    
    bool Contains(TK Token) {
        return util::vector::contains(Tokens, Token);
    }
};


template <typename T>
class MappedTokenSet {
    std::map<TK, T> Mapping;
    
public:
    MappedTokenSet(std::initializer_list<std::pair<TK, T>> Mapping) {
        for (auto &Pair : Mapping) {
            this->Mapping.insert(Pair);
        }
    }
    
    bool Contains(TK Token) {
        return Mapping.find(Token) != Mapping.end();
    }
    
    T &operator [](TK Token) {
        return Mapping.at(Token);
    }
};



#pragma mark - Token Collections

// The initial tokens of all binary operators (binops, comparisons, etc)
static TokenSet BinaryOperatorStartTokens = {
    TK::Plus, TK::Minus, TK::Asterisk, TK::ForwardSlash, TK::PercentageSign,
    TK::Ampersand, TK::Pipe, TK::Circumflex, TK::LessThanSign, TK::GreaterSign,
    TK::EqualsSign, TK::ExclamationMark
};



static MappedTokenSet<ast::BinaryOperation::Operation> SingleTokenBinopOperatorTokenMapping = {
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


#define save_pos(name) auto name = Position;
#define restore_pos(name) Position = name;

// How does the parser work?
//
// Position always points to the current token.
// For example, if we parse an identifier, after returning from `ParseIdentifier`, Position would point to the token after that identifier


TokenList LexFile(std::string &Path) {
    std::ifstream File(Path);
    std::ostringstream Contents;
    Contents << File.rdbuf();
    File.close();

    return Lexer().Lex(Contents.str(), Path);
}


AST Parser::Parse(std::string &FilePath) {
    this->Position = 0;
    this->Tokens = LexFile(FilePath);
    ImportedFiles.push_back(FilePath);
    
    AST Ast;
    while (Position < Tokens.size() && CurrentTokenKind() != TK::EOF_) {
        Ast.push_back(ParseTopLevelStmt());
    }
    
    return Ast;
}


#include <sys/stat.h>
namespace fs {
    bool file_exists(std::string &Path) {
        struct stat S;
        return stat(Path.c_str(), &S) == 0;
    }
}


// TODO move the entire module resolution stuff somewhere else !?
std::string Parser::ResolveImportPathRelativeToBaseDirectory(const std::string &ModuleName, const std::string &BaseDirectory) {
    if (ModuleName[0] == '/') { // absolute path
        return ModuleName;
    }
    
    std::string Path = std::string(BaseDirectory).append("/").append(ModuleName).append(".yo");
    if (fs::file_exists(Path)) return Path;
    
    LKFatalError("Unable to resolve import of '%s' relative to '%s'", ModuleName.c_str(), BaseDirectory.c_str());
}



void Parser::ResolveImport() {
    auto BaseDirectory = util::string::excludingLastPathComponent(CurrentToken().SourceLocation.Filename);
    assert_current_token_and_consume(TK::Use);
    
    auto ModuleName = ParseStringLiteral()->Value;
    assert_current_token_and_consume(TK::Semicolon);
    
    TokenList NewTokens;
    
    if (ModuleName[0] == ':') { // stdlib import
        if (util::vector::contains(ImportedFiles, ModuleName)) return;
        ImportedFiles.push_back(ModuleName);
        NewTokens = Lexer().Lex(stdlib_resolution::GetContentsOfModuleWithName(ModuleName), ModuleName);
    } else {
        std::string Path = ResolveImportPathRelativeToBaseDirectory(ModuleName, BaseDirectory);
        if (util::vector::contains(ImportedFiles, Path)) return;
        ImportedFiles.push_back(Path);
        NewTokens = LexFile(Path);
    }
    
    Tokens.insert(Tokens.begin() + Position,
                  NewTokens.begin(),
                  NewTokens.end() - 1); // exclude EOF_
}




std::shared_ptr<TopLevelStmt> Parser::ParseTopLevelStmt() {
    auto Annotations = ParseAnnotations();
    std::shared_ptr<TopLevelStmt> Stmt;
    
    switch (CurrentToken().Kind) {
        case TK::Fn:
            Stmt = ParseFunctionDecl();
            break;
        case TK::Extern:
            Stmt = ParseExternFunctionDecl();
            break;
        case TK::Struct:
            Stmt = ParseStructDecl();
            break;
        case TK::Impl:
            Stmt = ParseImplBlock();
            break;
        case TK::Use:
            ResolveImport(); // TODO can/should imports have annotations?
            return ParseTopLevelStmt();
        case TK::Using:
            Stmt = ParseTypealias();
            break;
        default: unhandled_token(CurrentToken());
    }
    
    Stmt->Annotations = Annotations;
    return Stmt;
}


std::shared_ptr<StructDecl> Parser::ParseStructDecl() {
    assert_current_token_and_consume(TK::Struct);
    
    auto Decl = std::make_shared<StructDecl>();
    Decl->Name = ParseIdentifier();
    
    if (CurrentTokenKind() == TK::LessThanSign) {
        Consume();
        while (CurrentTokenKind() != TK::GreaterSign) {
            Decl->TemplateArguments.push_back(ParseIdentifier()->Value);
            if (CurrentTokenKind() == TK::Comma) Consume();
        }
        assert_current_token_and_consume(TK::GreaterSign);
    }
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    Decl->Attributes = ParseParameterList();
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return Decl;
}


std::shared_ptr<ImplBlock> Parser::ParseImplBlock() {
    assert_current_token_and_consume(TK::Impl);
    
    auto Impl = std::make_shared<ImplBlock>(ParseIdentifier()->Value);
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    while (CurrentTokenKind() == TK::Fn) {
        Impl->Methods.push_back(ParseFunctionDecl());
    }
    
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return Impl;
}




std::vector<std::string> Parser::ParseAnnotations() {
    if (CurrentTokenKind() != TK::Hashtag) return {};
    Consume();
    assert_current_token_and_consume(TK::OpeningSquareBrackets);
    std::vector<std::string> Annotations;
    
    while (CurrentTokenKind() == TK::Identifier) {
        Annotations.push_back(ParseIdentifier()->Value);
        switch (CurrentTokenKind()) {
            case TK::Comma:
                Consume();
                continue;
            case TK::ClosingSquareBrackets:
                Consume();
                if (CurrentTokenKind() == TK::Hashtag) {
                    Consume(); continue;
                } else {
                    return Annotations;
                }
            default: unhandled_token(CurrentToken())
        }
    }
    
    return Annotations;
}



std::shared_ptr<FunctionSignature> Parser::ParseFunctionSignature(bool IsExternal) {
    assert_current_token_and_consume(TK::Fn);
    
    auto S = std::make_shared<ast::FunctionSignature>();
    S->Name = ParseIdentifier()->Value;
    
    if (CurrentTokenKind() == TK::LessThanSign) { // Template function
        S->IsTemplateFunction = true;
        Consume();
        while (CurrentTokenKind() != TK::GreaterSign) {
            S->TemplateArgumentNames.push_back(ParseIdentifier()->Value);
            if (CurrentTokenKind() == TK::Comma) Consume();
        }
        assert_current_token_and_consume(TK::GreaterSign);
    }
    assert_current_token_and_consume(TK::OpeningParens);
    
    if (!IsExternal) {
        S->Parameters = ParseParameterList();
    } else {
        S->Parameters = {};
        auto Ident = std::make_shared<Identifier>("");
        while (CurrentTokenKind() != TK::ClosingParens) {
            S->Parameters.push_back(std::make_shared<VariableDecl>(Ident, ParseType()));
            if (CurrentTokenKind() == TK::Comma) Consume();
        }
    }
    assert_current_token_and_consume(TK::ClosingParens);
    
    if (CurrentTokenKind() == TK::Colon) {
        Consume();
        S->ReturnType = ParseType();
    } else {
        S->ReturnType = TypeInfo::Void;
    }
    
    return S;
}



std::shared_ptr<ExternFunctionDecl> Parser::ParseExternFunctionDecl() {
    assert_current_token_and_consume(TK::Extern);
    
    auto EFD = std::make_shared<ExternFunctionDecl>();
    EFD->Signature = ParseFunctionSignature(true);
    
    assert_current_token_and_consume(TK::Semicolon);
    return EFD;
}

std::shared_ptr<FunctionDecl> Parser::ParseFunctionDecl() {
    auto FD = std::make_shared<FunctionDecl>();
    FD->Signature = ParseFunctionSignature(false);
    assert_current_token(TK::OpeningCurlyBraces);
    
    FD->Body = ParseComposite();
    return FD;
}





std::vector<std::shared_ptr<VariableDecl>> Parser::ParseParameterList() {
    std::vector<std::shared_ptr<VariableDecl>> Parameters;
    
    while (CurrentTokenKind() == TK::Identifier) {
        auto Ident = ParseIdentifier();
        assert_current_token_and_consume(TK::Colon);
        
        auto Type = ParseType();
        Parameters.push_back(std::make_shared<VariableDecl>(Ident, Type));
        
        if (CurrentTokenKind() == TK::Comma) {
            Consume();
        } else {
            break;
        }
    }
    
    return Parameters;
}


TypeInfo *Parser::ParseType() {
    if (CurrentTokenKind() == TK::Identifier) {
        auto Name = ParseIdentifier()->Value;
        return TypeInfo::GetWithName(Name);
    }
    if (CurrentTokenKind() == TK::Asterisk) {
        Consume();
        return ParseType()->getPointerTo();
    }
    return nullptr;
}



std::shared_ptr<ast::TypealiasDecl> Parser::ParseTypealias() {
    assert_current_token_and_consume(TK::Using);
    auto Name = ParseIdentifier()->Value;
    assert_current_token_and_consume(TK::EqualsSign);
    auto Type = ParseType();
    assert_current_token_and_consume(TK::Semicolon);
    return std::make_shared<TypealiasDecl>(Name, Type);
}



#pragma mark - Local Statements



std::shared_ptr<Composite> Parser::ParseComposite() {
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    auto C = std::make_shared<Composite>();
    while (CurrentTokenKind() != TK::ClosingCurlyBraces) {
        C->Statements.push_back(ParseLocalStmt());
    }
    
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    return C;
}






std::shared_ptr<LocalStmt> Parser::ParseLocalStmt() {
    if (CurrentTokenKind() == TK::Return) {
        return ParseReturnStmt();
    }
    
    if (CurrentTokenKind() == TK::Let) {
        return ParseVariableDecl();
    }
    
    if (CurrentTokenKind() == TK::If) {
        return ParseIfStmt();
    }
    
    if (CurrentTokenKind() == TK::While) {
        return ParseWhileStmt();
    }
    
    if (CurrentTokenKind() == TK::For) {
        return ParseForLoop();
    }
    
    std::shared_ptr<LocalStmt> S;
    std::shared_ptr<Expr> E; // A partially-parsed part of a local statement
    
    E = ParseMemberAccess();
    
    if (CurrentTokenKind() == TK::EqualsSign) { // Assignment
        Consume();
        auto Value = ParseExpression();
        assert_current_token_and_consume(TK::Semicolon);
        
        return std::make_shared<Assignment>(E, Value);
    }
    
    if (BinaryOperatorStartTokens.Contains(CurrentTokenKind())) {
        if (auto Op = ParseBinopOperator()) {
            assert_current_token_and_consume(TK::EqualsSign);
            
            auto Value = std::make_shared<BinaryOperation>(*Op, E, ParseExpression());
            S = std::make_shared<Assignment>(E, Value);
            assert_current_token_and_consume(TK::Semicolon);
            return S;
        }
    }
    
    if (CurrentTokenKind() == TK::Semicolon) {
        Consume();
        
        if (auto ExprAsLocalStmt = std::dynamic_pointer_cast<LocalStmt>(E)) {
            S = ExprAsLocalStmt;
        }
        
        return S;
    }
    
    unhandled_token(CurrentToken())
}



std::shared_ptr<ReturnStmt> Parser::ParseReturnStmt() {
    assert_current_token_and_consume(TK::Return);
    
    if (CurrentTokenKind() == TK::Semicolon) {
        Consume();
        return std::make_shared<ReturnStmt>(nullptr);
    }
    
    auto Expr = ParseExpression();
    assert_current_token_and_consume(TK::Semicolon);
    return std::make_shared<ReturnStmt>(Expr);
}



std::shared_ptr<VariableDecl> Parser::ParseVariableDecl() {
    assert_current_token_and_consume(TK::Let);
    
    auto Identifier = ParseIdentifier();
    auto Type = TypeInfo::Unresolved;
    std::shared_ptr<Expr> InitialValue;
    
    if (CurrentTokenKind() == TK::Colon) {
        Consume();
        Type = ParseType();
    }
    
    if (CurrentTokenKind() == TK::EqualsSign) {
        Consume();
        InitialValue = ParseExpression();
    }
    
    assert_current_token_and_consume(TK::Semicolon);
    
    return std::make_shared<VariableDecl>(Identifier, Type, InitialValue);
}



std::shared_ptr<IfStmt> Parser::ParseIfStmt() {
    using Kind = ast::IfStmt::Branch::BranchKind;
    assert_current_token_and_consume(TK::If);
    
    std::vector<std::shared_ptr<IfStmt::Branch>> Branches;
    
    auto MainExpr = ParseExpression();
    assert_current_token(TK::OpeningCurlyBraces);
    
    Branches.push_back(std::make_shared<IfStmt::Branch>(Kind::If,
                                                        MainExpr,
                                                        ParseComposite()));
    
    while (CurrentTokenKind() == TK::Else && PeekKind() == TK::If) {
        Consume(2);
        auto Expr = ParseExpression();
        assert_current_token(TK::OpeningCurlyBraces);
        auto Body = ParseComposite();
        Branches.push_back(std::make_shared<IfStmt::Branch>(Kind::ElseIf, Expr, Body));
    }
    
    if (CurrentTokenKind() == TK::Else && PeekKind() == TK::OpeningCurlyBraces) {
        Consume();
        Branches.push_back(std::make_shared<IfStmt::Branch>(Kind::Else, nullptr, ParseComposite()));
    }
    
    return std::make_shared<IfStmt>(Branches);
}




std::shared_ptr<ast::WhileStmt> Parser::ParseWhileStmt() {
    assert_current_token_and_consume(TK::While);
    
    auto Condition = ParseExpression();
    assert_current_token(TK::OpeningCurlyBraces);
    
    return std::make_shared<ast::WhileStmt>(Condition, ParseComposite());
}



std::shared_ptr<ForLoop> Parser::ParseForLoop() {
    assert_current_token_and_consume(TK::For);
    auto Ident = ParseIdentifier();
    assert_current_token_and_consume(TK::In);
    auto Expr = ParseExpression();
    assert_current_token(TK::OpeningCurlyBraces);
    auto Body = ParseComposite();
    return std::make_shared<ForLoop>(Ident, Expr, Body);
}



#pragma mark - Expressions





std::shared_ptr<Identifier> Parser::ParseIdentifier() {
    if (CurrentTokenKind() != TK::Identifier) return nullptr;
    auto R = std::make_shared<Identifier>(*CurrentToken().Data.S);
    Consume();
    return R;
}

        
std::optional<ast::BinaryOperation::Operation> Parser::ParseBinopOperator() {
    auto T = CurrentTokenKind();
    if (!BinaryOperatorStartTokens.Contains(T)) throw;
    
    if (SingleTokenBinopOperatorTokenMapping.Contains(T)) {
        Consume();
        return SingleTokenBinopOperatorTokenMapping[T];
    }
    
    if (T == TK::LessThanSign && PeekKind() == TK::LessThanSign) {
        Consume(2);
        return BinaryOperation::Operation::Shl;
    }
    
    if (T == TK::GreaterSign && PeekKind() == TK::GreaterSign) {
        Consume(2);
        return BinaryOperation::Operation::Shr;
    }
    
    return std::nullopt;
}


std::optional<ast::Comparison::Operation> Parser::ParseComparisonOperator() {
    using Op = ast::Comparison::Operation;
    
    auto Token = CurrentTokenKind();
    if (!BinaryOperatorStartTokens.Contains(Token)) throw;
    
    auto Next = PeekKind();
    
    if (Token == TK::EqualsSign && Next == TK::EqualsSign) {
        Consume(2); return Op::EQ;
    }
    
    if (Token == TK::ExclamationMark && Next == TK::EqualsSign) {
        Consume(2); return Op::NE;
    }
    
    if (Token == TK::LessThanSign && Next == TK::EqualsSign) {
        Consume(2); return Op::LE;
    }
    
    if (Token == TK::LessThanSign) {
        Consume(); return Op::LT;
    }
    
    if (Token == TK::GreaterSign && Next == TK::EqualsSign) {
        Consume(2); return Op::GE;
    }
    
    if (Token == TK::GreaterSign) {
        Consume(); return Op::GT;
    }
    
    return std::nullopt;
}


std::optional<ast::LogicalOperation::Operation> Parser::ParseLogicalOperationOperator() {
    auto Token = CurrentTokenKind();
    if (!BinaryOperatorStartTokens.Contains(Token)) throw;
    
    auto Next = PeekKind();
    
    if (Token == TK::Ampersand && Next == TK::Ampersand) {
        Consume(2); return ast::LogicalOperation::Operation::And;
    }
    if (Token == TK::Pipe && Next == TK::Pipe) {
        Consume(2); return ast::LogicalOperation::Operation::Or;
    }
    
    return std::nullopt;
}


PrecedenceGroup GetOperatorPrecedenceGroup(BinaryOperation::Operation Op) {
    switch (Op) {
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


PrecedenceGroup GetOperatorPrecedenceGroup(ast::Comparison::Operation) {
    return PrecedenceGroup::Comparison;
}

PrecedenceGroup GetOperatorPrecedenceGroup(ast::LogicalOperation::Operation Op) {
    switch (Op) {
        case LogicalOperation::Operation::And: return PrecedenceGroup::LogicalConjunction;
        case LogicalOperation::Operation::Or:  return PrecedenceGroup::LogicalDisjunction;
    }
}


// Tokens that, if they appear on their own, mark the end of an expression
static TokenSet ExpressionDelimitingTokens = {
    TK::ClosingParens, TK::Semicolon, TK::Comma, TK::OpeningCurlyBraces, TK::ClosingSquareBrackets
};


std::shared_ptr<Expr> Parser::ParseExpression(PrecedenceGroup PrecedenceGroupConstraint) {
    std::shared_ptr<Expr> E;
    
    if (CurrentTokenKind() == TK::OpeningParens) {
        Consume();
        E = ParseExpression();
        assert_current_token_and_consume(TK::ClosingParens);
    }
    
    if (!E) {
        E = ParseNumberLiteral();
    }
    
    if (!E) {
        E = ParseUnaryExpr();
    }
    
    if (!E) {
        // TODO support string literals as first elements in a ast::MemberAccess?
        E = ParseStringLiteral();
    }
    
    if (!E) {
        E = ParseMemberAccess();
    }
    
    while (BinaryOperatorStartTokens.Contains(CurrentTokenKind())) {
        save_pos(fallback)
        
        // Since there are multitple binary operators starting with the same initial token (`|` vs `||`, `<` vs `<<`, etc),
        // it's important we parse the different kinds of binary operators in the correct order
        
        if (auto Op = ParseLogicalOperationOperator()) {
            std::cout << E->Description() << std::endl;
            auto Op_Precedence = GetOperatorPrecedenceGroup(*Op);
            
            if (Op_Precedence > PrecedenceGroupConstraint) {
                auto RHS = ParseExpression(Op_Precedence);
                E = std::make_shared<LogicalOperation>(*Op, E, RHS);
            } else {
                restore_pos(fallback);
                return E;
            }
        
        } else if (auto Op = ParseBinopOperator()) {
            auto Op_Precedence = GetOperatorPrecedenceGroup(*Op);
            
            if (Op_Precedence > PrecedenceGroupConstraint) {
                auto RHS = ParseExpression(Op_Precedence);
                E = std::make_shared<BinaryOperation>(*Op, E, RHS);
            } else {
                restore_pos(fallback)
                return E;
            }
        
        } else if (auto Op = ParseComparisonOperator()) {
            auto RHS = ParseExpression(PrecedenceGroup::Comparison);
            // TODO: do we have to take the predecence group constraint into account?
            E = std::make_shared<Comparison>(*Op, E, RHS);
        
        } else {
            // We reach here if the current token is a binary operator starting token, but we didn't manage to parse a binop or a comparison
            throw;
        }
    }
    
    if (CurrentTokenKind() == TK::As) { // Typecast
        Consume();
        Typecast::CastKind Kind = Typecast::CastKind::StaticCast;
        if (CurrentTokenKind() == TK::ExclamationMark) {
            Kind = Typecast::CastKind::Bitcast;
            Consume();
        }
        
        auto Type = ParseType();
        E = std::make_shared<Typecast>(E, Type, Kind);
    }
    
    if (ExpressionDelimitingTokens.Contains(CurrentTokenKind())) {
        return E;
    }
    
    unhandled_token(CurrentToken())
}



static TokenSet MemberAccessSeparatingTokens = {
    TK::LessThanSign, TK::OpeningParens, TK::OpeningSquareBrackets, TK::Period, TK::Colon
};


// Returns either ast::MemberAccess or ast::Identifier
std::shared_ptr<Expr> Parser::ParseMemberAccess() {
    using MemberKind = MemberAccess::Member::MemberKind;
    
    std::vector<std::shared_ptr<MemberAccess::Member>> Members;
    
    // Initial Member
    auto Ident = ParseIdentifier();
    if (!Ident) throw;
    
    if (!MemberAccessSeparatingTokens.Contains(CurrentTokenKind())) {
        // Member Access that simply is a single identifier
        return Ident;
    }
    
    bool IsInitialIdentifier = true;
    
    while (MemberAccessSeparatingTokens.Contains(CurrentTokenKind())) {
        switch (CurrentTokenKind()) {
            case TK::LessThanSign:
            case TK::OpeningParens: {
                // We reach here in the following cases:
                // 1. foo() ...
                // 2. foo< ...
                // Issue: the 2nd case isn't necessarily a function call, it might very well just be an identifier that's part of a comparison
                
                bool IsTemplateArgumentListSpecfication = false;
                std::vector<TypeInfo *> ExplicitlySpecifiedTemplateArgumentTypes;
                
                if (CurrentTokenKind() == TK::LessThanSign) {
                    save_pos(pos_saved_after_ident);
                    IsTemplateArgumentListSpecfication = true;
                    Consume();
                    while (CurrentTokenKind() != TK::GreaterSign) { // TODO this might become a problem if we introduce an `<>` operator
                        auto Type = ParseType();
                        ExplicitlySpecifiedTemplateArgumentTypes.push_back(Type);
                        if (CurrentTokenKind() == TK::Comma) {
                            Consume(); continue;
                        } else if (CurrentTokenKind() == TK::GreaterSign) {
                            Consume(); break;
                        } else {
                            restore_pos(pos_saved_after_ident);
                            if (IsInitialIdentifier) {
                                return Ident;
                            } else {
                                return std::make_shared<MemberAccess>(Members);
                            }
                        }
                    }
                }
                
                if (CurrentTokenKind() != TK::OpeningParens) {
                    unhandled_token(CurrentToken());
                }
                Consume();
                
                auto Args = ParseExpressionList(TK::ClosingParens);
                assert_current_token_and_consume(TK::ClosingParens);
                auto Call = std::make_shared<ast::FunctionCall>(Ident, Args, false);
                Call->ExplicitTemplateArgumentTypes = ExplicitlySpecifiedTemplateArgumentTypes;
                Members.push_back(std::make_shared<MemberAccess::Member>(MemberKind::Initial_FunctionCall, Call));
                continue;
            }
            
            case TK::Colon: { // Static method call
                Consume();
                assert_current_token_and_consume(TK::Colon);
                auto CanonicalName = std::make_shared<Identifier>(mangling::MangleCanonicalName(Ident->Value, ParseIdentifier()->Value, FunctionSignature::FunctionKind::StaticMethod));
                assert_current_token_and_consume(TK::OpeningParens);
                auto Args = ParseExpressionList(TK::ClosingParens);
                assert_current_token_and_consume(TK::ClosingParens);
                Members.push_back(std::make_shared<MemberAccess::Member>(MemberKind::Initial_StaticCall,
                                                                         std::make_shared<FunctionCall>(CanonicalName, Args, false)));
                continue;
            }
            
            case TK::Period: {
                Consume();
                
                if (IsInitialIdentifier) {
                    Members.push_back(std::make_shared<MemberAccess::Member>(MemberKind::Initial_Identifier, Ident));
                    IsInitialIdentifier = false;
                }
                
                Ident = ParseIdentifier();
                if (CurrentTokenKind() == TK::OpeningParens) { // Method call
                    Consume();
                    auto Args = ParseExpressionList(TK::ClosingParens);
                    assert_current_token_and_consume(TK::ClosingParens);
                    Members.push_back(std::make_shared<MemberAccess::Member>(MemberKind::MemberFunctionCall,
                                                                             std::make_shared<FunctionCall>(Ident, Args, false)));
                } else { // Attribute Access
                    Members.push_back(std::make_shared<MemberAccess::Member>(MemberKind::MemberAttributeRead, Ident));
                }
                continue;
            }
            
            case TK::OpeningSquareBrackets: { // Subscript
                if (IsInitialIdentifier) {
                    Members.push_back(std::make_shared<MemberAccess::Member>(MemberKind::Initial_Identifier, Ident));
                    IsInitialIdentifier = false;
                }
                
                Consume();
                auto Offset = ParseExpression();
                assert_current_token_and_consume(TK::ClosingSquareBrackets);
                Members.push_back(std::make_shared<MemberAccess::Member>(MemberKind::OffsetRead, Offset));
                continue;
            }
            default: unhandled_token(CurrentToken())
        }
    }
    return std::make_shared<MemberAccess>(Members);
}






// Parses a (potentially empty) list of expressions separated by commas, until Delimiter is reached
// The delimiter is not consumed
std::vector<std::shared_ptr<Expr>> Parser::ParseExpressionList(Token::TokenKind Delimiter) {
    if (CurrentTokenKind() == Delimiter) return {};
    
    std::vector<std::shared_ptr<Expr>> Expressions;
    
    do {
        Expressions.push_back(ParseExpression());
        precondition(CurrentTokenKind() == TK::Comma || CurrentTokenKind() == Delimiter);
        if (CurrentTokenKind() == TK::Comma) Consume();
    } while (CurrentTokenKind() != Delimiter);
    
    assert_current_token(Delimiter);
    return Expressions;
}



std::shared_ptr<NumberLiteral> Parser::ParseNumberLiteral() {
    uint64_t Value;
    NumberLiteral::NumberType Type;
    bool IsNegated = false;
    
    save_pos(prev_pos)
    
    if (CurrentTokenKind() == TK::Minus) {
        Consume();
        IsNegated = true;
    }
    
    switch (CurrentTokenKind()) {
        case TK::IntegerLiteral:
            Value = CurrentToken().Data.I;
            Type = NumberLiteral::NumberType::Integer;
            break;
        case TK::DoubleLiteral: throw;
        case TK::CharLiteral:
            Value = CurrentToken().Data.C;
            Type = NumberLiteral::NumberType::Character;
            break;
        case TK::BoolLiteral:
            Value = CurrentToken().Data.C;
            Type = NumberLiteral::NumberType::Boolean;
            break;
        default:
            restore_pos(prev_pos);
            return nullptr;
    }
    Consume();
    
    if (IsNegated) {
        Value *= -1;
    }
    return std::make_shared<NumberLiteral>(Value, Type);
}




std::shared_ptr<StringLiteral> Parser::ParseStringLiteral() {
    auto &T = CurrentToken();
    
    if (T.Kind != TK::StringLiteral && T.Kind != TK::ByteStringLiteral) {
        return nullptr;
    }
    
    auto Value = *T.Data.S;
    StringLiteral::StringLiteralKind Kind = T.Kind == TK::StringLiteral
        ? StringLiteral::StringLiteralKind::NormalString
        : StringLiteral::StringLiteralKind::ByteString;
    
    Consume();
    return std::make_shared<StringLiteral>(Value, Kind);
}


static MappedTokenSet<ast::UnaryExpr::Operation> UnaryOperators = {
    { TK::Minus, UnaryExpr::Operation::Negate },
    { TK::Tilde, UnaryExpr::Operation::BitwiseNot },
    { TK::ExclamationMark, UnaryExpr::Operation::LogicalNegation}
};

std::shared_ptr<UnaryExpr> Parser::ParseUnaryExpr() {
    if (!UnaryOperators.Contains(CurrentTokenKind())) return nullptr;
    auto Op = UnaryOperators[CurrentTokenKind()];
    Consume();
    auto Expr = ParseExpression(PrecedenceGroup::PrefixOperator);
    return std::make_shared<UnaryExpr>(Op, Expr);
}
