//
//  11.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 11/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day11: Day {
    public static func run(input: [String]) {
        let serial = Int(input.first!)!
        var grid = Array(repeating: Array(repeating: 0, count: 300), count: 300)
        
        for x in 1...300 {
            for y in 1...300 {
                grid[x - 1][y - 1] = (((((x + 10) * y) + serial) * (x + 10)) / 100 % 10) - 5
            }
        }
        
        var highestSum = (x: 0, y: 0, sum: 0, size: 0)
        for size in 1...300 {
            for x in 1...(300 - size + 1) {
                for y in 1...(300 - size + 1) {
                    var sum = 0
                    for dx in 0..<size {
                        for dy in 0..<size {
                            sum = sum + grid[x + dx - 1][y + dy - 1]
                        }
                    }
                    if sum > highestSum.sum {
                        highestSum = (x: x, y: y, sum: sum, size: size)
                    }
                }
            }
            
            if size == 3 {
                print("highest 3x3 sum found at (\(highestSum.x), \(highestSum.y)) = \(highestSum.sum)")
            }
            
            if highestSum.size < size - 2 {
                break
            }
            
            print("done size \(size), highest sum = \(highestSum)")
        }
        
        print("highest sum found at (\(highestSum.x), \(highestSum.y)) = \(highestSum.sum), \(highestSum.size)x\(highestSum.size)")
    }
}
