//
//  18.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 19/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

enum Plot {
    case open
    case wooded
    case lumber
}

public class Day18: Day {
    static func summary(of yard: [Coord:Plot], at minute: Int) {
        let numTrees = yard.values.filter({ $0 == .wooded }).count
        let numLumberyards = yard.values.filter({ $0 == .lumber }).count
        let numResources = numTrees * numLumberyards
        print("After \(minute) minutes, there are \(numTrees) wooded acres and \(numLumberyards) lumberyards")
        print("\(numTrees) * \(numLumberyards) = \(numResources)")
        print()
    }
    
    public static func run(input: [String]) {
        var yard = [Coord:Plot]()
        
        for (y, row) in input.enumerated() {
            for (x, cell) in row.enumerated() {
                let coord = Coord(x: x, y: y)
                switch cell {
                case ".":
                    yard[coord] = .open
                case "#":
                    yard[coord] = .lumber
                case "|":
                    yard[coord] = .wooded
                default:
                    continue
                }
            }
        }
        
        let targetMinutes = 1000000000
        var yardHistory = [[Coord:Plot]]()
        yardHistory.append(yard)
        for i in 1...targetMinutes {
            var nextYard = yard
            for (coord, plot) in yard {
                switch plot {
                case .open:
                    let numTrees = (coord.adjacent() + coord.diagonal()).compactMap({ yard[$0] }).filter({ $0 == .wooded }).count
                    nextYard[coord] = numTrees >= 3 ? .wooded : .open
                case .wooded:
                    let numLumberyards = (coord.adjacent() + coord.diagonal()).compactMap({ yard[$0] }).filter({ $0 == .lumber }).count
                    nextYard[coord] = numLumberyards >= 3 ? .lumber : .wooded
                case .lumber:
                    let numTrees = (coord.adjacent() + coord.diagonal()).compactMap({ yard[$0] }).filter({ $0 == .wooded }).count
                    let numLumberyards = (coord.adjacent() + coord.diagonal()).compactMap({ yard[$0] }).filter({ $0 == .lumber }).count
                    nextYard[coord] = numLumberyards >= 1 && numTrees >= 1 ? .lumber : .open
                }
            }
            yard = nextYard
            
            if i == 10 {
                summary(of: yard, at: i)
            }
            
            if let lastFoundYardAt = yardHistory.lastIndex(of: yard) {
                print("duplicate yard found! \(i) minutes == \(lastFoundYardAt) minutes")
                summary(of: yard, at: i)
                summary(of: yardHistory[lastFoundYardAt], at: lastFoundYardAt)
                
                let diff = i - lastFoundYardAt
                let remaining = (targetMinutes - i) % diff
                
                summary(of: yardHistory[lastFoundYardAt + remaining], at: lastFoundYardAt + remaining)
                break
            }
            
            yardHistory.append(yard)
        }
    }
}
