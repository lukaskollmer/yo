//
//  CallStatRecorder.swift
//  yo
//
//  Created by Lukas Kollmer on 25.08.18.
//  Copyright © 2018 Lukas Kollmer. All rights reserved.
//

import Foundation


class CallStatsRecorder {
    private var stats = CountedSet<String>()
    
    func recordCallToFunction(withName name: String) {
        stats.insert(name)
    }
    
    
    func formattedStats() -> String {
        let sortedInvocationCounts = stats.backing
            .sorted {
                $0.value == $1.value
                    ? $0.key.caseInsensitiveCompare($1.key) == .orderedAscending
                    : $0.value > $1.value
            }
        
        let countMaxLength = sortedInvocationCounts[0].value.numberOfDigitsInBase10
        
        return sortedInvocationCounts
            .map { String($0.value).padding(.left, toLength: countMaxLength, withPad: " ") + " " + $0.key }
            .joined(separator: "\n")
    }
    
    func writeToFile(inDirectory directory: String = FileManager.default.currentDirectoryPath) {
        let dateString = DateFormatter.string(from: Date(), format: "yyyy-MM-dd_HHmmss")
        let filename = "yo_\(dateString).txt"
        let fullPath = directory.appending(pathComponent: filename)
        
        let data = formattedStats().data(using: .utf8)
        FileManager.default.createFile(atPath: fullPath, contents: data, attributes: nil)
    }
}
