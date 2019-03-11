//
//  Parser.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-03-03.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Parser.h"

#include <map>

using namespace ast;

using TK = Token::TokenKind;

#pragma mark - Parser Utils

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

AST Parser::Parse(TokenList Tokens) {
    this->Tokens = Tokens;
    this->Position = 0;
    
    AST Ast;
    while (Position < Tokens.size() && CurrentToken().getKind() != TK::EOF_) {
        Ast.push_back(ParseTopLevelStmt());
    }
    
    return Ast;
}

#define unhandled_token(T) { std::cout << "Unhandled Token: " << T << std::endl; throw; }


std::shared_ptr<TopLevelStmt> Parser::ParseTopLevelStmt() {
    switch (CurrentToken().getKind()) {
        case TK::Fn: return ParseFunctionDecl();
        case TK::Extern: return ParseExternFunctionDecl();
        case TK::Struct: return ParseStructDecl();
        default: unhandled_token(CurrentToken());
    }
}


std::shared_ptr<StructDecl> Parser::ParseStructDecl() {
    assert_current_token_and_consume(TK::Struct);
    
    auto Name = ParseIdentifier();
    assert_current_token_and_consume(TK::OpeningCurlyBraces);
    
    auto Attributes = ParseParameterList();
    assert_current_token_and_consume(TK::ClosingCurlyBraces);
    
    return std::make_shared<StructDecl>(Name, Attributes);
    
}


void Parser::ParseFunctionSignatureInto(std::shared_ptr<FunctionSignature> S) {
    assert_current_token_and_consume(TK::Fn);
    
    S->Name = ParseIdentifier()->Value;
    assert_current_token_and_consume(TK::OpeningParens);
    
    S->Parameters = ParseParameterList();
    assert_current_token_and_consume(TK::ClosingParens);
    
    if (CurrentTokenKind() == TK::Colon) {
        Consume();
        S->ReturnType = ParseType();
    } else {
        S->ReturnType = TypeInfo::Void;
    }
}



std::shared_ptr<ExternFunctionDecl> Parser::ParseExternFunctionDecl() {
    assert_current_token_and_consume(TK::Extern);
    
    auto EFD = std::make_shared<ExternFunctionDecl>();
    ParseFunctionSignatureInto(EFD);
    
    assert_current_token_and_consume(TK::Semicolon);
    
    return EFD;
}

std::shared_ptr<FunctionDecl> Parser::ParseFunctionDecl() {
    auto FD = std::make_shared<FunctionDecl>();
    
    ParseFunctionSignatureInto(FD);
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
        // TODO Is the `?:` operator standard c++ ? is it guaranteed that rhs will only be evaluated if lhs was nil?
        return TypeInfo::GetBuiltinWithName(Name) ?: TypeInfo::MakeComplex(Name);
    }
    if (CurrentTokenKind() == TK::Asterisk) {
        Consume();
        return TypeInfo::MakePointer(ParseType());
    }
    throw 0;
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




#pragma mark - Expressions





std::shared_ptr<Identifier> Parser::ParseIdentifier() {
    if (CurrentTokenKind() != TK::Identifier) return nullptr;
    auto R = std::make_shared<Identifier>(CurrentToken().getData().s);
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
        E = ParseMemberAccess();
    }
//    if (!E) {
//        E = ParseIdentifier();
//    }
//
//    if (std::dynamic_pointer_cast<Identifier>(E) && CurrentTokenKind() == TK::OpeningParens) {
//        // Function call
//        Consume();
//        auto Arguments = ParseExpressionList(TK::ClosingParens);
//        assert_current_token_and_consume(TK::ClosingParens);
//        E = std::make_shared<FunctionCall>(E, Arguments, false);
//    }
    
//    if (std::dynamic_pointer_cast<Identifier>(E) && CurrentTokenKind() == TK::OpeningSquareBrackets) {
        // Offset pointer read
//        Consume();
//        auto Offset = ParseExpression();
//        assert_current_token_and_consume(TK::ClosingSquareBrackets);
//        E = std::make_shared<PointerRead>(E, Offset);
//    }

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
    
    if (CurrentTokenKind() == TK::As) {
        // Typecast
        Consume();
        auto Type = ParseType();
        E = std::make_shared<Typecast>(E, Type);
    }
    
    if (ExpressionDelimitingTokens.Contains(CurrentTokenKind())) {
        return E;
    }
    
    unhandled_token(CurrentToken())
}



static TokenSet MemberAccessSeparatingTokens = {
    TK::OpeningParens, TK::OpeningSquareBrackets, TK::Period
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
    
    while (MemberAccessSeparatingTokens.Contains(CurrentTokenKind())) {
        switch (CurrentTokenKind()) {
            case Token::TokenKind::Period: throw;
                
            case Token::TokenKind::OpeningParens: { // Function Call
                Consume();
                auto Args = ParseExpressionList(TK::ClosingParens);
                assert_current_token_and_consume(TK::ClosingParens);
                auto M = std::make_shared<MemberAccess::Member>(MemberKind::Initial_FunctionCall);
                M->Data.Call = std::make_shared<FunctionCall>(Ident, Args, false);
                Members.push_back(M);
                break;
            }
                
                
            case Token::TokenKind::OpeningSquareBrackets: { // Pointer Read w/ offset
                
                auto M1 = std::make_shared<MemberAccess::Member>(MemberKind::Initial_Identifier);
                M1->Data.Ident = Ident;
                Members.push_back(M1);
                
                Consume(); // Consume the opening square brackets
                
                auto Offset = ParseExpression();
                assert_current_token_and_consume(TK::ClosingSquareBrackets);
                auto M2 = std::make_shared<MemberAccess::Member>(MemberKind::OffsetRead);
                M2->Data.Offset = Offset;
                Members.push_back(M2);
                break;
            }
            default:
                unhandled_token(CurrentTokenKind())
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
        assert_current_token_either({TK::Comma, Delimiter});
        if (CurrentTokenKind() == TK::Comma) Consume();
    } while (CurrentTokenKind() != Delimiter);
    
    assert_current_token(Delimiter);
    return Expressions;
}



std::shared_ptr<NumberLiteral> Parser::ParseNumberLiteral() {
    if (CurrentTokenKind() != TK::IntegerLiteral) {
        return nullptr;
    }
    
    auto Value = CurrentToken().getData().i;
    Consume();
    
    return std::make_shared<NumberLiteral>(Value);
}





