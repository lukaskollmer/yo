//
//  ASTFormatter.swift
//  yo
//
//  Created by Lukas Kollmer on 01.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

// TODO implement the rest of this

import Foundation

protocol ASTPrinterAppendable {
    var stringValue: String { get }
}

extension String: ASTPrinterAppendable {
    var stringValue: String {
        return self
    }
}

extension ASTIdentifier: ASTPrinterAppendable {
    var stringValue: String {
        return self.value
    }
}

extension ASTType: ASTPrinterAppendable {
    var stringValue: String {
        return self.typename
    }
}

class ASTPrinter {
    let indentationWidth: Int
    private var indentationLevel = 0
    private(set) var text = ""
    
    init(indentationWidth: Int) {
        self.indentationWidth = indentationWidth
    }
    
    @discardableResult
    func append(_ newText: ASTPrinterAppendable) -> ASTPrinter {
        text += newText.stringValue
        return self
    }
    
    // TODO what if we call newline multiple times in a row? remove all trailing whitespace!
    @discardableResult
    func newline() -> ASTPrinter {
        let indent = String(repeating: " ", count: indentationLevel * indentationWidth)
        text += "\n" + indent
        return self
    }
    
    @discardableResult
    func indent() -> ASTPrinter {
        indentationLevel += 1
        return self
    }
    
    @discardableResult
    func unindent() -> ASTPrinter {
        indentationLevel -= 1
        return self
    }
}

class ASTFormatter {
    private let ast: AST
    
    
    init(ast: AST) {
        self.ast = ast
    }
    
    func _print() {
        let printer = ASTPrinter(indentationWidth: 4)
        ast.forEach { print($0) }
        ast.forEach { $0.print(to: printer) }
        
        print(printer.text)
    }
}


extension ASTNode {
    func print(to printer: ASTPrinter) {
        fatalError("not yet implemented for \(type(of: self))")
    }
}


extension ASTNoop {
    func print(to printer: ASTPrinter) {
        // pass
    }
}


extension ASTImportStatement {
    func print(to printer: ASTPrinter) {
        printer.append("use \"\(self.moduleName)\";")
    }
}


extension ASTFunctionDeclaration {
    func print(to printer: ASTPrinter) {
        printer
            .append("fn ")
            .append(self.name)
            .append("(")
        
        for parameter in self.parameters {
            printer
                .append(parameter.identifier)
                .append(": ")
                .append(parameter.type)
        }
        printer
            .append("): ")
            .append(self.returnType.typename)
            .append(" ")
        self.body.print(to: printer)
        printer.newline()
    }
}


extension ASTComposite {
    func print(to printer: ASTPrinter) {
        printer
            .append("{")
            .indent()
            .newline()
        
        self.statements.forEach { $0.print(to: printer) }
        
        printer
            .unindent()
            .newline()
            .append("}")
        
    }
}


extension ASTReturnStatement {
    func print(to printer: ASTPrinter) {
        printer.append("ret ")
        self.expression.print(to: printer)
        printer.append(";")
    }
}


extension ASTNumberLiteral {
    func print(to printer: ASTPrinter) {
        printer.append("\(self.value)")
    }
}


extension ASTConditionalStatement {
    func print(to printer: ASTPrinter) {
        switch self.kind {
        case .while:
            printer.append("while ")
        case .if(_):
            printer.append("if ")
        }
        
        self.condition.print(to: printer)
        printer.append(" ")
        self.body.print(to: printer)
        
        if case .if(let elseBranch) = self.kind, elseBranch != nil {
            printer.append(" else ")
            elseBranch!.print(to: printer)
        }
        
        printer.newline().newline()
        
    }
}


extension ASTComparison {
    func print(to printer: ASTPrinter) {
        self.lhs.print(to: printer)
        printer.append(" ").append(self.operation).append(" ")
        self.rhs.print(to: printer)
    }
}

extension ASTComparison.Operation: ASTPrinterAppendable {
    var stringValue: String {
        switch self {
        case .equal:
            return "=="
        case .notEqual:
            return "!="
        case .less:
            return "<"
        case .greater:
            return ">"
        case .lessEqual:
            return "<="
        case .greaterEqual:
            return ">="
        }
    }
}
