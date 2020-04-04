//
//  MatchMaker.h
//  yo
//
//  Created by Lukas Kollmer on 2020-04-03.
//  Copyright © 2020 Lukas Kollmer. All rights reserved.
//

#pragma once

#include "parse/AST.h"

#include "llvm/IR/IRBuilder.h"

#include <vector>
#include <memory>
#include <cstdint>


namespace yo {
namespace irgen {

class IRGenerator;
enum ValueKind : uint8_t;

class MatchMaker {
    enum class PatternKind;
    
    struct PatternInfo {
        PatternKind kind;
        const lex::SourceLocation &loc;
        
        PatternInfo(PatternKind k, const lex::SourceLocation &l) : kind(k), loc(l) {}
    };
    
    
    IRGenerator &irgen;
    std::shared_ptr<ast::MatchExpr> matchExpr;
    ValueKind VK;
    
    // type of the expression we're matching against
    Type *targetType = nullptr;
    llvm::Value *targetV = nullptr;
    std::vector<std::vector<PatternKind>> patternKinds;
    
public:
    explicit MatchMaker(IRGenerator &irgen, std::shared_ptr<ast::MatchExpr> matchExpr, ValueKind VK)
    : irgen(irgen), matchExpr(matchExpr), VK(VK) {}
    
    /// Run codegen for a match expression
    llvm::Value* run();
    
private:
    void fetchPatternKinds();
    PatternKind getPatternKind(const ast::MatchExprPattern&);
    
    /// validate all patterns in all branches. returns true if all patterns are valid
    bool validatePatterns();
    
    bool matchIsExhaustive();
    
    using BindingIdsT = std::vector<uint64_t>;
    BindingIdsT insertPatternBindingsIntoLocalScope(const ast::MatchExprPattern&, PatternKind);
    void removePatternBindingsFromLocalScope(const BindingIdsT&);
};



} // ns irgen
} // ns yo

