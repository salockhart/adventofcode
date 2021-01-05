//
//  25.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 26/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day25: Day {
    public static func run(input: [String]) {
        let coords = input.map({ line -> Coord in
            let pieces = line.split(separator: ",").map({ Int(String($0))! })
            return Coord(x: pieces[0], y: pieces[1], z: pieces[2], t: pieces[3])
        })
        
        var connections = [Coord:[Coord]]()
        for coord in coords {
            for closeCoord in coords {
                if coord.manhattan(to: closeCoord) <= 3 {
                    connections[coord] = (connections[coord] ?? []) + [closeCoord]
                }
            }
        }
        
        var constellations = 0
        var visited = [Coord]()
        
        while visited.count != coords.count {
            print("visited \(visited.count) nodes - \(coords.count - visited.count) remaining")
            var next = [Coord]()
            var current = coords.first(where: { !visited.contains($0) })
            while let node = current {
                print("visiting \(node)")
                visited.append(node)
                next.append(contentsOf: (connections[node] ?? []).filter({ !visited.contains($0) && !next.contains($0) }))
                current = next.popLast()
            }
            constellations += 1
            print("constellations = \(constellations)")
        }
        
        print("there are \(constellations) constellations formed by the fixed points in spacetime")
    }
}
