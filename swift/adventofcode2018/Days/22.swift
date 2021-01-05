//
//  22.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 22/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day22: Day {
    public static func run(input: [String]) {
        let depth = Int(input[0][String.Index(encodedOffset: 7)..<String.Index(encodedOffset: input[0].count)])!
        let targetX = Int(input[1][String.Index(encodedOffset: 8)..<input[1].firstIndex(of: ",")!])!
        let targetY = Int(input[1][input[1].index(after: input[1].firstIndex(of: ",")!)..<String.Index(encodedOffset: input[1].count)])!
        let target = Coord(x: targetX, y: targetY)
        print(depth)
        print(target)
        
        var erosionLevels = [Coord:Int]()
        for y in 0...(target.y + 5) {
            for x in 0...(target.x + 5) {
                let coord = Coord(x: x, y: y)
                if (y == 0 && x == 0) || (coord == target) {
                    erosionLevels[coord] = (0 + depth) % 20183
                } else if y == 0 {
                    erosionLevels[coord] = ((x * 16807) + depth) % 20183
                } else if x == 0 {
                    erosionLevels[coord] = ((y * 48271) + depth) % 20183
                } else {
                    let left = coord + Coord(x: -1, y: 0)
                    let up = coord + Coord(x: 0, y: -1)
                    let leftErosion = erosionLevels[left]!
                    let rightErosion = erosionLevels[up]!
                    erosionLevels[coord] = ((leftErosion * rightErosion) + depth) % 20183
                }
            }
        }
        
        let riskLevel = erosionLevels
            .filter({ (0...target.x).contains($0.key.x) && (0...target.y).contains($0.key.y) })
            .values
            .map({ $0 % 3 })
            .reduce(0, +)
        print("the risk level =", riskLevel)
        
        enum Tool {
            case torch
            case gear
            case nothing
        }
        
        struct Option: Hashable, Equatable {
            let coord: Coord
            let tool: Tool
        }
        
        var options = [Coord:[Option]]()
        for coord in erosionLevels.keys {
            let option = coord
                .adjacent({ erosionLevels[$0] != nil })
                .flatMap({ adjacentCoord -> [Option] in
                    guard let erosion = erosionLevels[adjacentCoord] else {
                        fatalError("no erosion level for \(adjacentCoord)!")
                    }
                    switch erosion % 3 {
                    case 0: // rocky
                        return [Option(coord: adjacentCoord, tool: .gear), Option(coord: adjacentCoord, tool: .torch)]
                    case 1: // wet
                        return [Option(coord: adjacentCoord, tool: .gear), Option(coord: adjacentCoord, tool: .nothing)]
                    case 2: // narrow
                        return [Option(coord: adjacentCoord, tool: .torch), Option(coord: adjacentCoord, tool: .nothing)]
                    default:
                        return []
                    }
                });
            
            options[coord] = option
        }
        
        let getNeighbours = { (option: Option) -> [Option] in
            return options[option.coord]!
        }
        
        let getWeight = { (from: Option, to: Option) -> Int in
            return from.tool == to.tool ? 1 : 8
        }
        
        let paths = dijkstra(from: Option(coord: Coord(x: 0, y: 0), tool: .torch),
                             to: Option(coord: target, tool: .torch),
                             count: erosionLevels.count * 2,
                             neighbours: getNeighbours,
                             weight: getWeight)
        
        guard let path = paths[Option(coord: target, tool: .torch)] else {
            fatalError("no path to target!")
        }
        print("it takes \(path.distance) minutes to reach the target")
    }
}
