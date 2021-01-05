//
//  1.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 01/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day1: Day {
    public static func run(input: [String]) {
        var frequency = 0
        var set = Set([0])
        for line in input {
            frequency = frequency + (Int(line) ?? 0)
            set.insert(frequency)
        }
        
        print(frequency)
        
        var found = false
        while !found {
            for line in input {
                frequency = frequency + (Int(line) ?? 0)
                if set.contains(frequency) {
                    print("frequency repeated! \(frequency)")
                    found = true
                    break
                }
            }
        }
    }
}
