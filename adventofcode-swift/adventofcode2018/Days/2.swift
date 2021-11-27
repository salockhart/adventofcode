//
//  2.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 02/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day2: Day {
    public static func run(input: [String]) {
        let components = input
            .map { input -> [Character:Int] in
                var dict = [Character:Int]()
                for char in input {
                    dict[char] = (dict[char] ?? 0) + 1
                }
                return dict
            }
            .reduce((two: 0, three: 0)) { current, freq in
                return (
                    two: current.two + (freq.values.contains(2) ? 1 : 0),
                    three: current.three + (freq.values.contains(3) ? 1 : 0)
                )
            }
        
        print(components.two * components.three)
        
        let product = input.flatMap { left in
            return input.map { right in
                return (left: left, right: right)
            }
        }
        
        let targetLength = input[0].count
        
        let correctPair = product
            .lazy
            .map { (pair: (left: String, right: String)) -> String in
                return zip(pair.left, pair.right)
                    .filter { $0.0 == $0.1 }
                    .reduce("") { $0 + String($1.0) }
            }
            .first { common in
                return common.count == targetLength - 1
        }
        
        print(correctPair ?? "")
    }
}
