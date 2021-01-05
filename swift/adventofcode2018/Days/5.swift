//
//  5.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 05/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day5: Day {
    public static func run(input: [String]) {
        var stack = [String]()
        
        func handle(item: String) {
            if stack.count > 0
                && stack.last?.lowercased() == item.lowercased()
                && stack.last != item {
                
                _ = stack.popLast()
            } else {
                stack.append(item)
            }
        }
        
        for char in input[0] {
            handle(item: String(char))
        }
        
        print("reduced length = \(stack.joined().count)")
        
        let reducedInput = stack.joined()
        
        var bestLetter = ("", Int.max)
        
        for letter in "abcdefghijklmnopqrstuvwxyz" {
            stack = []
            for char in reducedInput where String(char).lowercased() != String(letter).lowercased() {
                handle(item: String(char))
            }
            if stack.count < bestLetter.1 {
                bestLetter = (String(letter), stack.count)
            }
        }
        
        print("best letter = \(bestLetter)")
    }
}
