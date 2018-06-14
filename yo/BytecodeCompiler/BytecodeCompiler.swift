//
//  Compiler.swift
//  yo
//
//  Created by Lukas Kollmer on 24.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation

// MARK: Constants
// TODO only use these throughout the file
private let retain  = SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "retain")
private let release = SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "release")


// MARK: Errors
enum BytecodeCompilerError: Error {
    // Function calls
    case undefinedFunction(ASTFunctionCall)
    case wrongNumberOfArgumentsPassedToFunction(ASTFunctionCall)
    
    
    case other(String, ASTNode)
}


// MARK: Codegen

/// Class that compiles an AST to bytecode instructions
class BytecodeCompiler {
    
    private var instructions = [WIPInstruction]()
    private var _counter = 0 // counter used to generate unique labels for if/while statements
    private func getCounter() -> Int {
        _counter += 1
        return _counter
    }
    
    
    // Scope info
    private var scope = Scope(type: .global)
    private var typeCache = TypeCache()
    private var functions = [String: SemanticAnalyzer.FunctionInfo]()
    
    private var breakDestination: String?
    private var continueDestination: String?
    
    
    
    init() {
        // fill the functions table w/ all native functions
        for builtin in Runtime.builtins {
            functions[builtin.name] = (
                argc: builtin.argc,
                parameterTypes: [], // TODO
                returnType: .primitive(name: "int") // TODO
            )
        }
    }
    
    
    func compile(ast: [ASTNode]) throws -> [WIPInstruction] {
        var importedPaths = [String]()
        
        // why is this a local function, insetad of a closure?
        // closured can't be recursive, but functions can
        func resolveImports(in ast: [ASTNode]) throws -> [ASTNode] {
            return try ast.lk_flatMap { node -> [ASTNode] in
                if let importStatement = node as? ASTImportStatement {
                    let path = ImportPathResolver.resolve(moduleName: importStatement.moduleName)
                    guard !importedPaths.contains(path) else { return [] }
                    
                    importedPaths.append(path)
                    return try resolveImports(in: try yo.tokenize(atPath: path))
                }
                return [node]
            }
        }
        
        var ast = try resolveImports(in: ast)
        
        // perform semantic analysis
        let semanticAnalysis = SemanticAnalyzer().analyze(ast: ast)
        
        // TODO ARC
        //ast = ARCAnalyzer(semanticAnalysis: semanticAnalysis).foo(ast: ast)
        
        
        // add the bootstrapping instructions
        add(.push, unresolvedLabel: "main")
        add(.call, 0)
        add(.push, -1)
        add(.jump, unresolvedLabel: "end")
        
        
        // import semantic analysis results
        self.functions.insert(contentsOf: semanticAnalysis.globalFunctions)
        semanticAnalysis.types.forEach(self.typeCache.register)
        
        // run codegen
        try ast.forEach(handle)
        
        add(label: "end")
        return instructions
        
    }
}

// MARK: Codegen
private extension BytecodeCompiler {
    
    func add(_ operation: Operation, _ immediate: Int = 0) {
        instructions.append(.operation(operation, immediate))
    }
    
    func add(label: String) {
        instructions.append(.label(label))
    }
    
    func add(_ operation: Operation, unresolvedLabel: String) {
        instructions.append(.unresolved(operation, unresolvedLabel))
    }
    
    
    // updates the scope until `block` returns
    func withScope(_ newScope: Scope, block: () throws -> Void) rethrows {
        let previousScope = scope
        scope = newScope
        
        try block()
        
        scope = previousScope
    }
}

private extension BytecodeCompiler {
    
    func handle(node: ASTNode) throws {
        
        if let function = node as? ASTFunctionDeclaration {
            try handle(function: function)
            
        } else if let returnStatement = node as? ASTReturnStatement {
            try handle(return: returnStatement)
            
        } else if let numberLiteral = node as? ASTNumberLiteral {
            try handle(numberLiteral: numberLiteral)
            
        } else if let functionCall = node as? ASTFunctionCall {
            try handle(functionCall: functionCall)
            
        } else if let binop = node as? ASTBinaryOperation {
            try handle(binop: binop)
            
        } else if let identifier = node as? ASTIdentifier {
            try handle(identifier: identifier)
            
        } else if let unary = node as? ASTUnary {
            try handle(unary: unary)
            
        } else if let composite = node as? ASTComposite {
            try handle(composite: composite)
            
        } else if let assignment = node as? ASTAssignment {
            try handle(assignment: assignment)
            
        } else if let _ = node as? ASTVariableDeclaration {
            // we don't need to explicitly handle variable declarations since that info is also passed in the ASTFunctionDeclaration node
            // TODO we might be able to remove all variable declarations from the a function's body statements?
            
        } else if let conditionalStatement = node as? ASTConditionalStatement {
            try handle(conditionalStatement: conditionalStatement)
            
        } else if let comparison = node as? ASTComparison {
            try handle(comparison: comparison)
            
        } else if let typeDeclaration = node as? ASTTypeDeclaration {
            try handle(typeDeclaration: typeDeclaration)
            
        } else if let arraySetter = node as? ASTArraySetter {
            try handle(arraySetter: arraySetter)
            
        } else if let arrayGetter = node as? ASTArrayGetter {
            try handle(arrayGetter: arrayGetter)
            
        } else if let memberGetter = node as? ASTTypeMemberGetter {
            try handle(memberGetter: memberGetter)
            
        } else if let typeImplementation = node as? ASTTypeImplementation {
            try handle(typeImplementation: typeImplementation)
            
        } else if let typeMemberFunctionCall = node as? ASTTypeMemberFunctionCall {
            try handle(typeMemberFunctionCall: typeMemberFunctionCall)
            
        } else if let memberSetter = node as? ASTTypeMemberSetter {
            try handle(memberSetter: memberSetter)
            
        } else if let _ = node as? ASTImportStatement {
            
        } else if let stringLiteral = node as? ASTStringLiteral {
            try handle(stringLiteral: stringLiteral)
            
        } else if let rawInstruction = node as? ASTRawWIPInstruction {
            instructions.append(rawInstruction.instruction)
            
        } else if let arrayLiteral = node as? ASTArrayLiteral {
            try handle(arrayLiteral: arrayLiteral)
            
        } else if let breakStatement = node as? ASTBreakStatement {
            try handle(breakStatement: breakStatement)
            
        } else if let continueStatement = node as? ASTContinueStatement {
            try handle(continueStatement: continueStatement)
            
        } else if let _ = node as? ASTNoop {
            
        } else {
            fatalError("unhandled node \(node)")
        }
        
    }
    
    
    
    // MARK: Handle Statements
    
    func handle(typeDeclaration: ASTTypeDeclaration) throws {
        
        let typename = typeDeclaration.name.name

        // generate an initializer
        let _self = ASTIdentifier(name: "self")
        let initializer = ASTFunctionDeclaration(
            name: ASTIdentifier(name: "init"),
            parameters: typeDeclaration.attributes,
            returnType: .complex(name: typeDeclaration.name.name), //typeDeclaration.name,
            kind: .staticImpl(typename),
            body: [
                // 1. declare self
                ASTVariableDeclaration(identifier: _self, type: .primitive(name: "int")), // TODO ARC: declare as int to avoid refcounting and accidentally deallocating before we even return from the initializer?
                
                // 2. allocate space on the heap
                ASTAssignment(
                    target: _self,
                    value: ASTFunctionCall(
                        functionName: SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "alloc"),
                        arguments: [ASTNumberLiteral(value: typeDeclaration.attributes.count + 1)],
                        unusedReturnValue: false)
                ),
                
                // set the address of the type's dealloc function
                // since we have to resolve this at compile time, we push the address of the function onto the stack, then use a noop as the value expression
                // TODO if the type did not define a dealloc function, set it to -1 to indicate that?
                
                
                // push the type's dealloc address onto the stack and shift it 41 to the left
                ASTRawWIPInstruction(instruction: .operation(.push, 40)),
                ASTRawWIPInstruction(instruction: .unresolved(.push, SymbolMangling.mangleInstanceMember(ofType: typename, memberName: "dealloc"))),
                ASTRawWIPInstruction(instruction: .operation(.shl, 0)),
                
                // push the type's id onto the stack
                ASTRawWIPInstruction(instruction: .operation(.push, typeCache.index(ofType: typename))),
                
                // combine dealloc address and type id
                ASTRawWIPInstruction(instruction: .operation(.or, 0)),
                
                // set the type's dealloc address and type id (from the steps above) in its first field
                ASTArraySetter(target: _self, offset: ASTNumberLiteral(value: 0), value: ASTNoop()),
                
                // go through the parameters and fill the attributes
                ASTComposite(
                    statements: typeDeclaration.attributes.enumerated().map {
                        ASTArraySetter(target: _self, offset: ASTNumberLiteral(value: $0.offset + 1), value: $0.element.identifier)
                    }
                ),
                
                //ASTFunctionCall(functionName: retain, arguments: [_self], unusedReturnValue: true),
                
                // 4. return the newly created object
                ASTReturnStatement(returnValueExpression: _self)
            ]
        )
        
        try handle(function: initializer)
    }
    
    
    func handle(function: ASTFunctionDeclaration) throws {
        guard function.body.statements.getLocalVariables(recursive: true).intersection(with: function.parameters).isEmpty else {
            fatalError("local variable cannot (yet?) shadow parameter")
        }
        
        try withScope(self.scope.withType(.function(name: function.mangledName, returnType: function.returnType), newParameters: function.parameters)) {
            // function entry point
            add(label: function.mangledName)
            
            // retain all arguments
            // TODO ARC
            //insertCalls(to: retain, forObjectsIn: scope.parameters)
            
            // Generate instructions for the function body
            // if the function doesn't have a return statement, we implicitly return 0
            
            let functionBody: ASTComposite
            if !(function.body.statements.last is ASTReturnStatement) {
                functionBody = ASTComposite(
                    statements: function.body.statements + [ASTReturnStatement(returnValueExpression: ASTNumberLiteral(value: 0))],
                    introducesNewScope: true
                )
                print("added ret 0 to \(function.mangledName)")
            } else {
                functionBody = function.body
            }
            
            try handle(composite: functionBody)
        }
    }
    
    
    func handle(composite: ASTComposite) throws {
        guard case .function(let functionName, let returnType) = scope.type else {
            fatalError("top level composite outside a function?")
        }
        
        // if the composite doesn't introduce a new scope, we simply handle all statements
        if !composite.introducesNewScope {
            try composite.statements.forEach(handle)
            return
        }
        
        // if the composite _does_ introduce a new scope, we:
        // 1. allocate space on the stack for the new variables
        // 2. handle all statements
        // 3. insert `runtime::release` calls for all non-primitive variables declared in the composite
        // TODO: what if we return in the composite? we need to release all local variables, not just the ones declared within the composite!!!
        let hasReturnStatement = composite.statements.any { $0 is ASTReturnStatement }
        var localVariables = composite.statements.getLocalVariables(recursive: false)
        
        // only used if `hasReturnStatement == true`
        let retval_temp_storage = ASTVariableDeclaration(
            identifier: ASTIdentifier(name: "__retval_\(functionName)"),
            type: returnType
        )
        
        if hasReturnStatement {
            localVariables.append(retval_temp_storage)
        }
        
        try withScope(scope.adding(localVariables: localVariables)) {
            add(.alloc, localVariables.count)
            
            if !hasReturnStatement {
                try composite.statements.forEach(handle)
                let localVariables = composite.statements.getLocalVariables(recursive: false)
                //insertCalls(to: release, forObjectsIn: localVariables)
                
                for _ in 0..<localVariables.count { add(.pop) }
            } else {
                // the composite contains a return statement
                // this means that we need to insert release calls for all objects in the local scope before handling the return statement
                
                for statement in composite.statements {
                    if let returnStatement = statement as? ASTReturnStatement {
                        
                        //let returnsLocalVariable = // TODO?
                        
                        let storeRetval = ASTAssignment(
                            target: retval_temp_storage.identifier,
                            value: returnStatement.returnValueExpression,
                            shouldRetainAssignedValueIfItIsAnObject: false//(returnStatement.returnValueExpression is ASTIdentifier)
                        )
                        
                        try handle(assignment: storeRetval)
                        //insertCalls(to: retain, forObjectsIn: [retval_temp_storage])
                        //insertCalls(to: release, forObjectsIn: [retval_temp_storage])
                        
                        
                        //print(scope.localVariables + scope.parameters)
                        //insertCalls(to: release, forObjectsIn: scope.localVariables + scope.parameters)
                        
                        //let objectsToBeReleased = (scope.localVariables + scope.parameters).filter { $0.identifier != retval_temp_storage.identifier }
                        //print(objectsToBeReleased)
                        //insertCalls(to: release, forObjectsIn: objectsToBeReleased)
                        
                        // Problem: what if we return one of the local variables? (or one of its attributes) (or it's somehow used in the return value expression)?
                        // idea: create a local vatiable for the return value (w/ the type of the function's return type), exclude that from all the release calls and return that
                        //handle(return: returnStatement)
                        try handle(return: ASTReturnStatement(returnValueExpression: retval_temp_storage.identifier))
                        
                    } else {
                        try handle(node: statement)
                    }
                }
            }
            
        }
    }
    
    
    func handle(return returnStatement: ASTReturnStatement) throws {
        try handle(node: returnStatement.returnValueExpression)
        add(.ret, scope.size)
    }
    
    func handle(arraySetter: ASTArraySetter) throws {
        try handle(node: arraySetter.value)
        try handle(node: arraySetter.offset)
        try handle(node: arraySetter.target)
        
        add(.storeh)
    }
    
    func handle(arrayGetter: ASTArrayGetter) throws {
        try handle(node: arrayGetter.offset)
        try handle(node: arrayGetter.target)
        
        add(.loadh)
    }
    
    func handle(typeImplementation: ASTTypeImplementation) throws {
        for function in typeImplementation.functions {
            try handle(function: function)
        }
    }
    
    func handle(typeMemberFunctionCall: ASTTypeMemberFunctionCall) throws {
        guard case .complex(let typename) = try scope.type(of: typeMemberFunctionCall.target.name) else {
            fatalError("trying to call method on non complex type")
        }
        
        let call = ASTFunctionCall(
            functionName: SymbolMangling.mangleInstanceMember(ofType: typename, memberName: typeMemberFunctionCall.functionName.name),
            arguments: [typeMemberFunctionCall.target] + typeMemberFunctionCall.arguments,
            unusedReturnValue: typeMemberFunctionCall.unusedReturnValue
        )
        
        try handle(functionCall: call)
    }
    
    
    
    
    
    
    
    func handle(conditionalStatement: ASTConditionalStatement) throws {
        guard case .function(let functionName, _) = scope.type else {
            fatalError("global if statement")
        }
        
        let counter = getCounter()
        let generateLabel: (String) -> String = { "\(functionName)_ifwhile_\(counter)_\($0)" } // TOOD replace `ifwhile` w/ just if or while?
        
        let oldBreakDestination = breakDestination
        let oldContinueDestination = continueDestination
        
        
        // 1. handle the condition
        // we only need a label for the condition if this is a while statement
        if case .while = conditionalStatement.kind {
            add(label: generateLabel("cond"))
            
            // for loops are also while statements (at least for the time being?)
            self.breakDestination = generateLabel("end")
            self.continueDestination = generateLabel("cond")
        }
        try handle(condition: conditionalStatement.condition)
        
        // 2. handle the jump to the body if the condition is true
        // if the condition is false, we fall through to jump to the else branch (or the end, if there is no else branch)
        add(.jump, unresolvedLabel: generateLabel("body"))
        
        // 3. handle the else jump
        // if this is a while loop, we just jump to the end
        // if this is an if statement that has an else branch, we jump there, otherwise to the end
        add(.push, -1)
        if case .if(let elseBranch) = conditionalStatement.kind, elseBranch != nil {
            add(.jump, unresolvedLabel: generateLabel("else"))
        } else {
            add(.jump, unresolvedLabel: generateLabel("end"))
        }
        
        
        // 4. handle the body
        add(label: generateLabel("body"))
        try handle(composite: conditionalStatement.body)
        
        // depending on whether this is an if or while statement, we jump to the end (if) or the condition (while)
        add(.push, -1)
        if case .while = conditionalStatement.kind {
            add(.jump, unresolvedLabel: generateLabel("cond"))
        } else {
            add(.jump, unresolvedLabel: generateLabel("end"))
        }
        
        // 5. if this is an if statement w/ an else branch, handle the else branch
        if case .if(let elseBranch) = conditionalStatement.kind, let elseBranch_ = elseBranch {
            add(label: generateLabel("else"))
            try handle(composite: elseBranch_)
        }
        
        // 6. handle the end label
        add(label: generateLabel("end"))
        
        if case .while = conditionalStatement.kind {
            self.breakDestination = oldBreakDestination
            self.continueDestination = oldContinueDestination
        }
    }
    
    func handle(breakStatement: ASTBreakStatement) throws {
        guard let breakDestination = breakDestination else {
            fatalError("ugh something went wrong")
        }
        
        add(.push, -1)
        add(.jump, unresolvedLabel: breakDestination)
    }
    
    func handle(continueStatement: ASTContinueStatement) throws {
        guard let continueDestination = continueDestination else {
            fatalError("ugh sorry for that")
        }
        
        add(.push, -1)
        add(.jump, unresolvedLabel: continueDestination)
    }
    
    
    func handle(assignment: ASTAssignment) throws {
        guard let target = assignment.target as? ASTIdentifier else {
            fatalError("can only assign to variables as of right now")
        }
        
        let targetIsObject = try scope.isObject(identifier: target.name)
        
        if targetIsObject {
            // TODO release the old value?
        }
        
        try handle(node: assignment.value)
        add(.store, try scope.index(of: target.name))
        
        // TODO ARC
        return
        
        if assignment.shouldRetainAssignedValueIfItIsAnObject && targetIsObject {
            let retainCall = ASTFunctionCall(
                functionName: SymbolMangling.mangleStaticMember(ofType: "runtime", memberName: "retain"),
                arguments: [target],
                unusedReturnValue: true
            )
            try handle(functionCall: retainCall)
        }
        
    }
    
    
    
    
    
    // MARK: Handle Expressions
    
    func handle(functionCall: ASTFunctionCall) throws {
        let isGlobalFunction = functions.keys.contains(functionCall.functionName)
        
        let argc: Int
        if isGlobalFunction {
            guard let _argc = functions[functionCall.functionName]?.argc else {
                //fatalError("trying to call non-existent function")
                throw BytecodeCompilerError.undefinedFunction(functionCall)
            }
            argc = _argc // seems like we can't directly assign to argc bc that's non-optional
        } else {
            guard case ASTType.function(let returnType, let parameterTypes) = try scope.type(of: functionCall.functionName) else {
                throw BytecodeCompilerError.undefinedFunction(functionCall)
            }
            argc = parameterTypes.count
        }
        
        guard argc == functionCall.arguments.count else {
            //fatalError("wrong argc")
            throw BytecodeCompilerError.wrongNumberOfArgumentsPassedToFunction(functionCall)
        }
        
        // push arguments on the stack
        for arg in functionCall.arguments.reversed() {
            try handle(node: arg)
        }
        
        
        // push the address onto the stack
        if let builtin = Runtime.builtin(withName: functionCall.functionName) {
            add(.push, builtin.address)
        } else if isGlobalFunction {
            add(.push, unresolvedLabel: SymbolMangling.mangleGlobalFunction(name: functionCall.functionName))
        } else {
            try handle(identifier: ASTIdentifier(name: functionCall.functionName))
        }
        
        // call w/ the passed number of arguments
        add(.call, functionCall.arguments.count)
        
        if functionCall.unusedReturnValue {
            add(.pop)
        }
    }
    
    
    func handle(identifier: ASTIdentifier) throws {
        if let index = try? scope.index(of: identifier.name) {
            // local variable
            add(.load, index)
            
        } else if functions.keys.contains(identifier.name) {
            // global function
            add(.push, unresolvedLabel: identifier.name)
        } else {
            fatalError("trying to use unknown idenfifier")
        }
    }
    
    func handle(memberGetter: ASTTypeMemberGetter) throws {
        guard case .complex(let typename) = try scope.type(of: memberGetter.target.name) else {
            fatalError("tryong to access member of non-complex type")
        }
        let membername = memberGetter.memberName.name
        
        guard typeCache.type(typename, hasMember: membername) else {
            fatalError("type '\(typename)' doesn't have member '\(membername)'")
        }
        
        let offset = typeCache.offset(ofMember: membername, inType: typename)
        add(.push, offset)
        try handle(identifier: memberGetter.target)
        add(.loadh)
    }
    
    
    func handle(memberSetter: ASTTypeMemberSetter) throws {
        guard case .complex(let typename) = try scope.type(of: memberSetter.target.name) else {
            fatalError("tryong to set member of non-complex type")
        }
        let membername = memberSetter.memberName.name
        
        guard typeCache.type(typename, hasMember: membername) else {
            fatalError("type '\(typename)' doesn't have member '\(membername)'")
        }
        
        try handle(node: memberSetter.newValue)
        
        let offset = typeCache.offset(ofMember: membername, inType: typename)
        add(.push, offset)
        
        try handle(identifier: memberSetter.target)
        add(.storeh)
    }
    
    
    func handle(binop: ASTBinaryOperation) throws {
        try handle(node: binop.rhs)
        try handle(node: binop.lhs)
        
        add(binop.operation.operation)
    }
    
    func handle(unary: ASTUnary) throws {
        // TODO add support for NOT
        try handle(binop: ASTBinaryOperation(lhs: ASTNumberLiteral(value: -1), operation: .mul, rhs: unary.expression))
    }
    
    
    func handle(numberLiteral: ASTNumberLiteral) throws {
        add(.push, numberLiteral.value)
    }
    
    func handle(stringLiteral: ASTStringLiteral) throws {
        let value = stringLiteral.value
        
        let codepoints: [Int] = value.unicodeScalars.map { Int($0.value) }
        
        let label = UUID().uuidString // TODO detect duplicate string literals
        instructions.append(.arrayLiteral(label, codepoints))
        
        add(.loadc, unresolvedLabel: label)
        
        let stringInitCall = ASTFunctionCall(
            functionName: SymbolMangling.mangleInitializer(forType: "String"),
            arguments: [ASTNoop()], // the parameter is already on the stack, from the `loadc` instruction above
            unusedReturnValue: false
        )
        
        try handle(functionCall: stringInitCall)
    }
    
    
    func handle(arrayLiteral: ASTArrayLiteral) throws {
        // TODO if the array is just number literals, store it as a constant (like strings)
        // otherwise, just generate a bunch of Array.add calls? (that wouldn't work inline)
        
        let isConstant = arrayLiteral.elements.all { $0 is ASTNumberLiteral }
        
        if isConstant {
            let values = arrayLiteral.elements.map { ($0 as! ASTNumberLiteral).value }
            let label = UUID().uuidString
            instructions.append(.arrayLiteral(label, values))
            
            add(.loadc, unresolvedLabel: label)
            
            let initCall = ASTFunctionCall(
                functionName: SymbolMangling.mangleStaticMember(ofType: "Array", memberName: "_fromConstantLiteral"),
                arguments: [ASTNoop()], // the parameter is already on the stack, from the `loadc` instruction above
                unusedReturnValue: false
            )
            try handle(functionCall: initCall)
            return
        }
        
        fatalError("array literals for non-constant values not yet implemented")
    }
    
    
    
    func handle(condition: ASTCondition) throws {
        if let comparison = condition as? ASTComparison {
            try handle(comparison: comparison)
            
        } else if let binaryCondition = condition as? ASTBinaryCondition {
            try handle(binaryCondition: binaryCondition)
        } else {
            fatalError("unhandled condition \(condition)")
        }
    }
    
    
    func handle(comparison: ASTComparison) throws {
        try handle(node: comparison.rhs)
        try handle(node: comparison.lhs)
        
        switch comparison.operator {
        case .equal:
            add(.eq)
        case .notEqual:
            add(.eq)
            add(.not)
        case .less:
            add(.lt)
        case .greater:
            add(.le)
            add(.not)
        case .lessEqual:
            add(.le)
        case .greaterEqual:
            add(.lt)
            add(.not)
        }
    }
    
    
    func handle(binaryCondition: ASTBinaryCondition) throws {
        // evaluate both conditions (lhs and rhs), order doesn't matter
        try handle(node: binaryCondition.lhs)
        try handle(node: binaryCondition.rhs)
        
        // the last two values on the stack are now one of these options:
        // -1, -1   (lhs: true  | rhs: true )
        // -1,  0   (lhs: true  | rhs: false)
        //  0, -1   (lhs: false | rhs: true )
        //  0,  0   (lhs: false | rhs: false)
        
        // we now add the last two entries on the stack
        // if the result is -2, both are true
        // if the result is -1, one of them is true
        // if the result is  0, both are false
        
        let expectedResult = binaryCondition.operator == .and ? -2 : -1
        
        add(.add)
        add(.push, expectedResult)
        add(.eq)
    }
}


// MARK: Helpers
private extension BytecodeCompiler {
    func insertCalls(to functionName: String, forObjectsIn variables: [ASTVariableDeclaration]) throws {
        fatalError("ugh")
        let identifiers = try variables
            .map    { $0.identifier }
            .filter { try self.scope.isObject(identifier: $0.name) }
        
        for identifier in identifiers {
            let call = ASTFunctionCall(
                functionName: functionName,
                arguments: [identifier],
                unusedReturnValue: true
            )
            try self.handle(functionCall: call)
        }
    }
}
