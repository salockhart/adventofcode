//
//  20.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 21/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

enum FloorType {
    case floor
    case wall
    case door
}

public class Day20: Day {
    public static func run(input: [String]) {
        var current = Coord(x: 0, y: 0)
        var prev = current
        var map = [Coord:[Coord]]()
        var positions = [Coord]()
        var distances = [Coord:Int]()
        
        let movements: [Character:Coord] = [
            "N": Coord(x: 0, y: 1),
            "S": Coord(x: 0, y: -1),
            "E": Coord(x: 1, y: 0),
            "W": Coord(x: -1, y: 0)
        ]
        
        for direction in input.first![String.Index(encodedOffset: 1)..<String.Index(encodedOffset: input.first!.count - 1)] {
//            print(direction, positions)
            
            switch direction {
            case "(":
                positions.append(current)
            case ")":
                current = positions.popLast()!;
            case "|":
                current = positions.last!
            default:
                current = current + movements[direction]!
                map[current] = (map[current] ?? []) + [prev]
                
                if (distances[current] ?? 0) != 0 {
                    distances[current] = min(distances[current] ?? 0, (distances[prev] ?? 0) + 1)
                } else {
                    distances[current] = (distances[prev] ?? 0) + 1
                }
            }
            
            prev = current
        }
        
        print("the longest path through the base is", distances.values.max()!)
        print(distances.values.filter({ $0 >= 1000 }).count, "rooms pass through at least 1000 doors")
    }
}
