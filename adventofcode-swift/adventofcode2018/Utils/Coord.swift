//
//  Coord.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 13/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public struct Coord: Hashable {
    let x: Int
    let y: Int
    let z: Int
    let t: Int
    private let hasZ: Bool
    private let hasT: Bool
    
    public init(x: Int, y: Int) {
        self.x = x
        self.y = y
        self.z = 0
        self.t = 0
        self.hasZ = false
        self.hasT = false
    }
    
    public init(x: Int, y: Int, z: Int) {
        self.x = x
        self.y = y
        self.z = z
        self.t = 0
        self.hasZ = true
        self.hasT = false
    }
    
    public init(x: Int, y: Int, z: Int, t: Int) {
        self.x = x
        self.y = y
        self.z = z
        self.t = t
        self.hasZ = true
        self.hasT = true
    }
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
        hasher.combine(z)
        hasher.combine(t)
    }
    
    public static func ==(lhs: Coord, rhs: Coord) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z && lhs.t == rhs.t
    }
}

extension Coord {
    public static var zero: Coord {
        return Coord(x: 0, y: 0, z: 0, t: 0)
    }
    
    func adjacent(_ isIncluded: (Coord) -> Bool = { _ in true }) -> [Coord] {
        return [
            Coord(x: x - 1, y : y),
            Coord(x: x + 1, y : y),
            Coord(x: x, y : y - 1),
            Coord(x: x, y : y + 1)
        ].filter(isIncluded)
    }
    
    func diagonal(_ isIncluded: (Coord) -> Bool = { _ in true }) -> [Coord] {
        return [
            Coord(x: x - 1, y : y - 1),
            Coord(x: x + 1, y : y - 1),
            Coord(x: x - 1, y : y + 1),
            Coord(x: x + 1, y : y + 1)
        ].filter(isIncluded)
    }
    
    func manhattan(to: Coord) -> Int {
        let dx = abs(x - to.x)
        let dy = abs(y - to.y)
        let dz = abs(z - to.z)
        let dt = abs(t - to.t)
        return dx + dy + dz + dt
    }
}

extension Coord: CustomStringConvertible {
    public var description: String {
        return hasZ
            ? hasT
                ? "(\(x), \(y), \(z), \(t))"
                : "(\(x), \(y), \(z))"
            : "(\(x), \(y))"
    }
}

extension Coord {
    public static func +(lhs: Coord, rhs: Coord) -> Coord {
        return Coord(x: lhs.x + rhs.x, y: lhs.y + rhs.y, z: lhs.z + rhs.z, t: lhs.t + rhs.t)
    }
}
