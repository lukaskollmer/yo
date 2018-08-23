//
//  Profiling.swift
//  yo
//
//  Created by Lukas Kollmer on 22.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


struct Profiling {
    
    static private let isProfilingEnabled = CLI.hasFlag(.emitSignposts)
    
    // Instruments Points of Interest
    enum Event: UInt32 {
        case parseAST
        case compile
        case interpret
        case shutdown
    }
    
    static func record(event: Event, arg1: UInt = 0, arg2: UInt = 0, arg3: UInt = 0, arg4: UInt = 0) {
        guard isProfilingEnabled else { return }
        kdebug_signpost(event.rawValue, arg1, arg2, arg3, arg4)
    }
    
    static func recordStart(event: Event, arg1: UInt = 0, arg2: UInt = 0, arg3: UInt = 0, arg4: UInt = 0) {
        guard isProfilingEnabled else { return }
        kdebug_signpost_start(event.rawValue, arg1, arg2, arg3, arg4)
    }
    
    static func recordEnd(event: Event, arg1: UInt = 0, arg2: UInt = 0, arg3: UInt = 0, arg4: UInt = 0) {
        guard isProfilingEnabled else { return }
        kdebug_signpost_end(event.rawValue, arg1, arg2, arg3, arg4)
    }
    
    static func record(event: Event, arg1: UInt = 0, arg2: UInt = 0, arg3: UInt = 0, arg4: UInt = 0, block: () throws -> Void) rethrows {
        guard isProfilingEnabled else { return }
        recordStart(event: event, arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4)
        try block()
        recordEnd(event: event, arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4)
    }
}
