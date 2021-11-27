//
//  10.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 10/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

struct Point {
    var x: Int
    var y: Int
    let dx: Int
    let dy: Int
    
    func move() -> Point {
        return Point(x: x + dx, y: y + dy, dx: dx, dy: dy)
    }
    
    func moveBack() -> Point {
        return Point(x: x - dx, y: y - dy, dx: dx, dy: dy)
    }
}

extension Array where Element == Point {
    func domain() -> (min: Int, max: Int) {
        let domain = self.map({ $0.x }).sorted()
        return (min: domain.first!, max: domain.last!)
    }
    
    func range() -> (min: Int, max: Int) {
        let range = self.map({ $0.y }).sorted()
        return (min: range.first!, max: range.last!)
    }
    
    func area() -> Int {
        let (minX, maxX) = self.domain()
        let (minY, maxY) = self.range()
        
        return abs(maxX - minX) * abs(maxY - minY)
    }
    
    func display() {
        let (minX, maxX) = self.domain()
        let (minY, maxY) = self.range()
        
        for y in minY...maxY {
            for x in minX...maxX {
                if self.contains(where: { $0.x == x && $0.y == y }) {
                    print("#", terminator:"")
                } else {
                    print(".", terminator:"")
                }
            }
            print()
        }
    }
}

import Foundation

public class Day10: Day {
    public static func run(input: [String]) {
        var points = input
            .map({ $0.split(whereSeparator: { "< >,".contains($0) }) })
            .map({ Point(x: Int($0[1])!, y: Int($0[2])!, dx: Int($0[4])!, dy: Int($0[5])!) })
        
        var seconds = 0
        var area = points.area()
        while true {
            points = points.map({ $0.move() })
            seconds = seconds + 1
            let newArea = points.area()
            if area < newArea {
                break
            }
            area = newArea
        }
        
        points = points.map({ $0.moveBack() })
        points.display()
        print("seconds = \(seconds - 1)")
    }
}
