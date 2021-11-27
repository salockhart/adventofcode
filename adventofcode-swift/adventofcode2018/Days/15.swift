//
//  15.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 15/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

enum UnitType {
    case elf
    case goblin
}

class Unit: CustomStringConvertible {
    let type: UnitType
    var position: Coord
    var points = 200
    var power = 3
    var dead = false
    
    init(type: UnitType, position: Coord) {
        self.type = type
        self.position = position
    }
    
    var description: String {
        return "\(type) @ (\(position.x),\(position.y))"
    }
}

enum TileType {
    case floor
    case wall
    case unit(Unit)
}

extension Filtering {
    static func isOpenTile(map: [Coord:TileType]) -> (Coord) -> Bool {
        return { coord in
            guard let tile = map[coord] else { return false }
            switch tile {
            case .floor:
                return true
            case .wall:
                return false
            case .unit(_):
                return false
            }
        }
    }
    
    static func isEnemy(of type: UnitType, map: [Coord:TileType]) -> (Coord) -> Bool {
        return { coord in
            guard let tile = map[coord] else { return false }
            switch tile {
            case .floor:
                return false
            case .wall:
                return false
            case .unit(let unit):
                switch unit.type {
                case .elf:
                    return type == .goblin
                case .goblin:
                    return type == .elf
                }
            }
        }
    }
}

public class Day15: Day {
    static func corners(of map: [Coord:TileType]) -> (minX: Int, maxX: Int, minY: Int, maxY: Int) {
        let coords = map.keys.sorted(by: Sorting.readingOrder)
        return (minX: coords.first?.x ?? 0, maxX: coords.last?.x ?? 0, minY: coords.first?.y ?? 0, maxY: coords.last?.y ?? 0)
    }
    
    static func display(map: [Coord:TileType], replacement: ((Coord) -> String?)? = nil) {
        let (minX, maxX, minY, maxY) = corners(of: map)
        for y in minY...maxY {
            for x in minX...maxX {
                let coord = Coord(x: x, y: y)
                guard let tile = map[coord] else {
                    print(replacement?(coord) ?? " ", terminator: "")
                    continue
                }
                switch tile {
                case .floor:
                    print(replacement?(coord) ?? ".", terminator: "")
                case .wall:
                    print(replacement?(coord) ?? "#", terminator: "")
                case .unit(let unit):
                    switch unit.type {
                    case .elf:
                        print(replacement?(coord) ?? "E", terminator: "")
                    case .goblin:
                        print(replacement?(coord) ?? "G", terminator: "")
                    }
                }
            }
            print()
        }
        print()
    }
    
    static func canAttack(_ unit: Unit, enemies: [Unit], map: [Coord:TileType]) -> Unit? {
        let possibleEnemies = unit.position
            .adjacent(Filtering.isEnemy(of: unit.type, map: map))
            .compactMap({ coord in enemies.first(where: { $0.position == coord }) })
        
        let lowestHealth = possibleEnemies.map({ $0.points }).min()
        let viableEnemies = possibleEnemies.filter({ $0.points == lowestHealth })
        
        return viableEnemies
            .sorted(by: { Sorting.readingOrder($0.position, $1.position) })
            .first
    }
    
    public static func run(input: [String]) {
        var elfPower = 3
        while true {
            var elves = [Unit]()
            var goblins = [Unit]()
            var map = [Coord:TileType]()
            
            for (y, row) in input.enumerated() {
                for (x, cell) in row.enumerated() {
                    let coord = Coord(x: x, y: y)
                    switch cell {
                    case "#":
                        map[coord] = .wall
                    case ".":
                        map[coord] = .floor
                    case "G":
                        let goblin = Unit(type: .goblin, position: coord)
                        map[coord] = .unit(goblin)
                        goblins.append(goblin)
                    case "E":
                        let elf = Unit(type: .elf, position: coord)
                        elf.power = elfPower
                        map[coord] = .unit(elf)
                        elves.append(elf)
                    default:
                        break
                    }
                }
            }
            
            var targetElvesAlive = elves.count
            
            func move(_ unit: Unit, to destination: Coord) {
                map[unit.position] = .floor
                map[destination] = .unit(unit)
                unit.position = destination
            }
            
            func attack(_ unit: Unit, for damage: Int) {
//                print("attacking \(unit.type) for \(damage) dmg")
                unit.points -= damage
                
                if unit.points <= 0 {
                    unit.dead = true
                    map[unit.position] = .floor
                    
                    if unit.type == .elf {
                        elves.removeAll(where: { $0.points <= 0 })
                    } else {
                        goblins.removeAll(where: { $0.points <= 0 })
                    }
                }
            }
            
//            print("initial")
//            display(map: map)
            
            var round = 0
            var noTargetsFound = false
            while elves.count > 0 && goblins.count > 0 {
                let actionOrder = (elves + goblins).sorted(by: { Sorting.readingOrder($0.position, $1.position) })
//                print(actionOrder)
                
                for unit in actionOrder {
//                    print("starting with \(unit)")
                    if unit.dead {
//                        print("\(unit) is dead")
                        continue
                    }
                    let enemies = (unit.type == .elf ? goblins : elves)
                    if enemies.count == 0 {
//                        print("\(unit) has no possible targets")
                        noTargetsFound = true
                    }
                    
                    if let attackingTarget = canAttack(unit, enemies: enemies, map: map) {
//                        print("attacking with \(unit)")
                        attack(attackingTarget, for: unit.power)
                    } else {
//                        print("trying to move \(unit)")
                        let squaresInRange = Set(enemies.flatMap({ $0.position.adjacent(Filtering.isOpenTile(map: map)) }))
//                        display(map: map) { squaresInRange.contains($0) ? "?" : nil }
                        
                        let reachable = dijkstra(from: unit.position,
                                                 count: map.count,
                                                 neighbours: { $0.adjacent(Filtering.isOpenTile(map: map)) },
                                                 tiebreaker: { Sorting.readingOrder($0[1], $1[1]) }
                            )
                            .filter({ squaresInRange.contains($0.key) })
                        
//                        display(map: map) { reachable.keys.contains($0) ? "@" : nil }
                        
                        let shortestDistance = reachable.map({ $0.value.distance }).min()
                        let options = reachable.filter({ $0.value.distance == shortestDistance })
//                        display(map: map) { options.keys.contains($0) ? "!" : nil }
                        
                        guard let destination = options.sorted(by: { Sorting.readingOrder($0.key, $1.key) }).first, destination.value.path.count > 1 else {
//                            print("\(unit) has no movement options")
                            continue
                        }
                        
//                        display(map: map) { $0 == destination.key ? "+" : nil }
                        
//                        print("moving \(unit) to \(destination.value.path[1])")
                        move(unit, to: destination.value.path[1])
//                        display(map: map)
                        
                        if let attackingTarget = canAttack(unit, enemies: enemies, map: map) {
                            attack(attackingTarget, for: unit.power)
                        }
                    }
                }
                
                round = round + 1
//                print("round: \(round)")
//                display(map: map)
            }
            
//            print("final")
//            display(map: map)
            
            let remainingHealth = (elves + goblins).map({ $0.points }).reduce(0, +)
            let lastFullRound = noTargetsFound ? round - 1 : round
            
            print("Elves have attack power \(elfPower) - \(targetElvesAlive - elves.count) died")
            print("Combat ends after \(lastFullRound) full rounds")
            print("\(elves.count > 0 ? "Elves" : "Goblins") win with \(remainingHealth) total hit points left")
            print("Outcome: \(lastFullRound) * \(remainingHealth) = \(lastFullRound * remainingHealth)")
            print()
            
            if (elves.count == targetElvesAlive) {
                break
            }
            
            elfPower = elfPower + 1
        }
    }
}
