//
//  test.swift
//  test
//
//  Created by Lukas Kollmer on 30.05.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import XCTest

class test: XCTestCase {
    
    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    
    func readTestSource(_ filename: String, path: String = #file) -> String {
        
        let directory = NSString(string: path).deletingLastPathComponent
        return try! yo.read(file: directory.appending(pathComponent: filename))
    }
    
    // MARK: Tests
    
    func testPrimitiveReturnValue() {
        XCTAssertEqual(42, try! yo.run(code: readTestSource("src/primitiveReturnValue.yo")))
    }
    
    
    func testFunctionReturnTypeRequired() {
        XCTAssertThrowsError(try yo.tokenize(code: readTestSource("src/functionReturnType.yo")))
    }
    
    func testFunctionCall() {
        XCTAssertEqual(12, try! yo.run(code: readTestSource("src/functionCall.yo")))
    }
    
    func testBinaryOperations() {
        XCTAssertEqual(0, try! yo.run(code: readTestSource("src/binop.yo")))
    }
    
}
