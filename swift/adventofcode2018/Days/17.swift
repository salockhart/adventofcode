//
//  17.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 18/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

enum Soil {
    case spring
    case clay
    case dry
    case wet
    case water
}

public class Day17: Day {
    static func display(map: [Coord:Soil], replace: ((Coord) -> String?)? = nil) {
        let minX = map.keys.map({ $0.x }).min()! - 1
        let maxX = map.keys.map({ $0.x }).max()! + 1
        let minY = map.keys.map({ $0.y }).min()! - 1
        let maxY = map.keys.map({ $0.y }).max()! + 1
//        let maxY = (map.filter({ [.wet, .water].contains($0.value) }).keys.map({ $0.y }).max() ?? 0) + 10
//        let minY = max(map.keys.map({ $0.y }).min()!, maxY - 50)
        
        for y in minY...maxY {
            for x in minX...maxX {
                let coord = Coord(x: x, y: y)
                
                if let replacement = replace?(coord) {
                    print(replacement, terminator: "")
                    continue
                }
                
                let type = map[coord] ?? .dry
                switch (type) {
                case .spring:
                    print("+", terminator: "")
                case .clay:
                    print("#", terminator: "")
                case .dry:
                    print(" ", terminator: "")
                case .wet:
                    print("|", terminator: "")
                case .water:
                    print("~", terminator: "")
                }
            }
            print()
        }
        print(Array(repeating: "-", count: 100).joined())
        usleep(100000)
    }
    
    public static func run(input: [String]) {
        var mapping = [Coord:Soil]()
        let springCoords = Coord(x: 500, y: 0)
        
        var clayCoords = input
            .map({ $0.split(whereSeparator: { "=,. ".contains($0) }) })
            .map({ line -> [Coord] in
                if line[0] == "x" {
                    let x = Int(line[1])!
                    let minY = Int(line[3])!
                    let maxY = Int(line[4])!
                    return (minY...maxY).map({ Coord(x: x, y: $0) })
                } else {
                    let y = Int(line[1])!
                    let minX = Int(line[3])!
                    let maxX = Int(line[4])!
                    return (minX...maxX).map({ Coord(x: $0, y: y) })
                }
            })
            .reduce([], +)
        
        for clayCoord in clayCoords {
            mapping[clayCoord] = .clay
        }
        
//        display(map: mapping)
        
        var count = 0
        let minY = mapping.keys.map({ $0.y }).min()!
        let maxY = mapping.keys.map({ $0.y }).max()!
        mapping[springCoords] = .spring
        var streams = [springCoords]
        while streams.count > 0 {
            count += 1
            
//            display(map: mapping) { coord in
//                return streams.contains(coord) ? "V" : nil
//            }
            
            streams = streams.flatMap({ stream -> [Coord] in
                if stream != springCoords {
                    mapping[stream] = .wet
                }
                
                let above = stream + Coord(x: 0, y: -1)
                let below = stream + Coord(x: 0, y: 1)
                let left = stream + Coord(x: -1, y: 0)
                let twiceLeft = stream + Coord(x: -2, y: 0)
                let right = stream + Coord(x: 1, y: 0)
                let twiceRight = stream + Coord(x: 2, y: 0)
                
                if (mapping[stream] ?? .dry) == .water {
                    return [above]
                }
                
                switch mapping[below] ?? .dry {
                case .clay:
                    fallthrough
                case .water:
                    let walls = mapping.filter({ $0.key.y == stream.y && $0.value == .clay })
                    
                    let leftWalls = walls.filter({ $0.key.x < stream.x })
                    let rightWalls = walls.filter({ $0.key.x > stream.x })
                    
                    if let leftWall = leftWalls.sorted(by: { Sorting.readingOrder($0.key, $1.key) }).last,
                        let rightWall = rightWalls.sorted(by: { Sorting.readingOrder($0.key, $1.key) }).first,
                        !((leftWall.key.x + 1)..<(rightWall.key.x)).contains(where: { ![.water, .clay].contains(mapping[Coord(x: $0, y: stream.y + 1)] ?? .dry) }) {
                        
//                        display(map: mapping) { coord in
//                            coord == leftWall.key
//                                ? "L"
//                                : coord == rightWall.key
//                                    ? "R"
//                                    : leftWalls.keys.contains(coord)
//                                        ? "l"
//                                        : rightWalls.keys.contains(coord)
//                                            ? "r"
//                                            : nil
//                        }
                        
                        for x in (leftWall.key.x + 1)..<(rightWall.key.x) {
                            mapping[Coord(x: x, y: stream.y)] = .water
                        }
                        
                        
                        return [above]
                    } else {
                        let splits = [left, right].filter({
                            (mapping[$0] ?? .dry) == .dry
                                || ((mapping[$0] ?? .dry) == .wet && (mapping[Coord(x: $0.x + 1, y: $0.y)] ?? .dry) == .dry)
                                || ((mapping[$0] ?? .dry) == .wet && (mapping[Coord(x: $0.x - 1, y: $0.y)] ?? .dry) == .dry)
                        })
                        
//                        if count > 300 {
//                            display(map: mapping) { coord in
//                                return streams.contains(coord)
//                                    ? "V"
//                                    : splits.contains(coord)
//                                    ? "*"
//                                    : nil
//                            }
//                        }
                        
                        return splits
                    }
                default:
                    break
                }
                
                if stream.y >= maxY {
                    return []
                }
                
                return [below]
            })
        }
     
        display(map: mapping)
        
        let zone = mapping
            .filter({ $0.key.y <= maxY && $0.key.y >= minY })
            .values
        
        let waterTiles = zone
            .filter({ $0 == .water })
            .count
        
        let wetTiles = zone
            .filter({ $0 == .wet })
            .count
        
        print("\(waterTiles + wetTiles) m^2 are accessible by the water")
        
        print("\(waterTiles) m^2 have still water")
    }
}
