//
//  6.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 06/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day6: Day {
    public static func run(input: [String]) {
        let coords = input
            .map { $0.split(separator: ",") }
            .map { $0.map { $0.trimmingCharacters(in: .whitespaces) } }
            .map { (x: Int($0[0]) ?? 0, y: Int($0[1]) ?? 0) }
        
        var count = 0
        var areas = [(count: Int, infinite: Bool)](repeating: (0, false), count: coords.count)
        let minX = coords.map({ $0.x }).min()!
        let maxX = coords.map({ $0.x }).max()!
        let minY = coords.map({ $0.y }).min()!
        let maxY = coords.map({ $0.y }).max()!
        
        for x in minX...maxX {
            for y in minY...maxY {
                let distances = coords.lazy.map({ ($0 - x).magnitude + ($1 - y).magnitude }).enumerated()
                if distances.map({ $0.element }).reduce(0, +) < 10000 {
                    count += 1
                }
                
                let minDistance = distances.min(by: { $0.element < $1.element })!.element
                let distancesAtMin = distances.filter({ $0.element == minDistance })
                if distancesAtMin.count > 1 { continue }
                if x == minX || x == maxX || y == minY || y == maxY {
                    areas[distancesAtMin[0].offset].infinite = true
                }
                areas[distancesAtMin[0].offset].count += 1
            }
        }
        
        print("largest area size = \(areas.lazy.filter({ !$0.infinite }).map({ $0.count }).max()!)")
        print("num coords > 10000 = \(count)")
    }
}
