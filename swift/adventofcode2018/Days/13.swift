//
//  13.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 13/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

struct Car: Equatable, Hashable {
    let id: Int
    let position: Coord
    let direction: Direction
    let junctions: Int
    
    func move(on tracks: [Coord:Track]) -> Car {
        var newDirection = direction
        var newPosition = position
        var newJunction = junctions
        
        switch direction {
        case .north:
            newPosition = Coord(x: position.x, y: position.y - 1)
            switch tracks[newPosition]! {
            case .rightTurn:
                newDirection = .east
            case .leftTurn:
                newDirection = .west
            case .junction:
                switch junctions % 3 {
                case 0:
                    newDirection = .west
                case 1:
                    newDirection = .north
                case 2:
                    newDirection = .east
                default:
                    newDirection = .north
                }
                newJunction = newJunction + 1
            default:
                break
            }
        case .south:
            newPosition = Coord(x: position.x, y: position.y + 1)
            switch tracks[newPosition]! {
            case .rightTurn:
                newDirection = .west
            case .leftTurn:
                newDirection = .east
            case .junction:
                switch junctions % 3 {
                case 0:
                    newDirection = .east
                case 1:
                    newDirection = .south
                case 2:
                    newDirection = .west
                default:
                    newDirection = .south
                }
                newJunction = newJunction + 1
            default:
                break
            }
        case .east:
            newPosition = Coord(x: position.x + 1, y: position.y)
            switch tracks[newPosition]! {
            case .rightTurn:
                newDirection = .north
            case .leftTurn:
                newDirection = .south
            case .junction:
                switch junctions % 3 {
                case 0:
                    newDirection = .north
                case 1:
                    newDirection = .east
                case 2:
                    newDirection = .south
                default:
                    newDirection = .east
                }
                newJunction = newJunction + 1
            default:
                break
            }
        case .west:
            newPosition = Coord(x: position.x - 1, y: position.y)
            switch tracks[newPosition]! {
            case .rightTurn:
                newDirection = .south
            case .leftTurn:
                newDirection = .north
            case .junction:
                switch junctions % 3 {
                case 0:
                    newDirection = .south
                case 1:
                    newDirection = .west
                case 2:
                    newDirection = .north
                default:
                    newDirection = .west
                }
                newJunction = newJunction + 1
            default:
                break
            }
        }
        
        return Car(id: id, position: newPosition, direction: newDirection, junctions: newJunction)
    }
    
    static func == (lhs: Car, rhs: Car) -> Bool {
        return lhs.id == rhs.id
    }
}

enum Direction {
    case north
    case south
    case east
    case west
}

enum Track {
    case straight(Direction)
    case rightTurn
    case leftTurn
    case junction
}

public class Day13: Day {
    static func display(tracks: [Coord:Track], cars: [Car]) {
        let minX = tracks.keys.map({ $0.x }).min()!
        let maxX = tracks.keys.map({ $0.x }).max()!
        let minY = tracks.keys.map({ $0.y }).min()!
        let maxY = tracks.keys.map({ $0.y }).max()!
        
        for y in minY...maxY {
            for x in minX...maxX {
                let cars = cars.filter({ $0.position.x == x && $0.position.y == y })
                if cars.count > 0 {
                    if cars.count > 1 {
                        print("X", terminator: "")
                    } else if let car = cars.first {
                        switch car.direction {
                        case .north:
                            print("^", terminator: "")
                        case .south:
                            print("v", terminator: "")
                        case .east:
                            print(">", terminator: "")
                        case .west:
                            print("<", terminator: "")
                        }
                    }
                } else if let track = tracks[Coord(x: x, y: y)] {
                    switch track {
                    case .straight(let direction):
                        switch direction {
                        case .north:
                            fallthrough
                        case .south:
                            print("|", terminator: "")
                        case .east:
                            fallthrough
                        case .west:
                            print("-", terminator: "")
                        }
                    case .rightTurn:
                        print("/", terminator: "")
                    case .leftTurn:
                        print("\\", terminator: "")
                    case .junction:
                        print("+", terminator: "")
                    }
                } else {
                    print(" ", terminator: "")
                }
            }
            print()
        }
        print()
    }
    
    public static func run(input: [String]) {
        
        var tracks = [Coord:Track]()
        var cars = [Car]()
        
        for (y, row) in input.enumerated() {
            for (x, col) in row.enumerated() {
                let coord = Coord(x: x, y: y)
                switch col {
                case "-":
                    tracks[coord] = .straight(.east)
                case "|":
                    tracks[coord] = .straight(.north)
                case "/":
                    tracks[coord] = .rightTurn
                case "\\":
                    tracks[coord] = .leftTurn
                case "+":
                    tracks[coord] = .junction
                case ">":
                    tracks[coord] = .straight(.east)
                    cars.append(Car(id: cars.count, position: coord, direction: .east, junctions: 0))
                case "<":
                    tracks[coord] = .straight(.west)
                    cars.append(Car(id: cars.count, position: coord, direction: .west, junctions: 0))
                case "^":
                    tracks[coord] = .straight(.north)
                    cars.append(Car(id: cars.count, position: coord, direction: .north, junctions: 0))
                case "v":
                    tracks[coord] = .straight(.south)
                    cars.append(Car(id: cars.count, position: coord, direction: .south, junctions: 0))
                default:
                    continue
                }
            }
        }
        
        var removed = [Car:Bool]()
        while cars.count > 1 {
//            display(tracks: tracks, cars: cars)
            let movementOrder = cars.sorted { (a, b) -> Bool in
                return Sorting.readingOrder(a.position, b.position)
            }

            var newCars = [Car]()
            for car in movementOrder {
                if (removed[car] == true) {
                    continue
                }
                let newCar = car.move(on: tracks)
                newCars.append(newCar)
                
                let otherCars = movementOrder.filter({ !newCars.contains($0) }) + newCars

                if let otherCar = otherCars.first(where: { $0.id != newCar.id && $0.position == newCar.position && removed[$0] != true }) {
//                    display(tracks: tracks, cars: otherCars)
                    print("collision found at \(newCar.position)")
                    removed[newCar] = true
                    removed[otherCar] = true
                }
            }

            cars = newCars.filter({ removed[$0] != true })
        }
        
        let lastCar = cars[0]//.move(on: tracks)
        print("last car standing = \(lastCar)")
    }
}
