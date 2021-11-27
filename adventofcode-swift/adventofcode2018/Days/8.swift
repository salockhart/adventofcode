//
//  8.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 08/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day8: Day {
    public static func run(input: [String]) {
        var stack = input
            .flatMap({ $0.split(separator: " ") })
            .map({ Int($0)! })
        
        func handleNode(nodeNumber: Int = 0) -> (sum: Int, value: Int) {
            var sum = 0
            var value = 0
            var childValues = [Int:Int]()
            var metadata = [Int]()
            let numChildren = stack.removeFirst()
            let numMetadata = stack.removeFirst()
            
            for i in 0..<numChildren {
                let (childSum, childValue) = handleNode(nodeNumber: nodeNumber + i + 1)
                childValues[i] = childValue
                sum += childSum
            }
            
            for _ in 0..<numMetadata {
                let item = stack.removeFirst()
                metadata.append(item)
                sum += item
            }
            
            if numChildren == 0 {
                value = sum
            } else {
                value = metadata.map({ childValues[$0 - 1] ?? 0 }).reduce(0, +)
            }
            
            return (sum, value)
        }
        
        let (sum, value) = handleNode()
        
        print("sum = \(sum)")
        
        print("value = \(value)")
    }
}
