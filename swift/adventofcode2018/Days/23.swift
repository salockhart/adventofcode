//
//  23.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 23/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day23: Day {
    struct Bot {
        let coord: Coord
        let radius: Int
    }
    
    public static func run(input: [String]) {
        let bots = input.map({ line -> Bot in
            let parts = line.split(whereSeparator: { "pos=<,> r=".contains($0) })
            return Bot(coord: Coord(x: Int(parts[0])!, y: Int(parts[1])!, z: Int(parts[2])!), radius: Int(parts[3])!)
        })
        
        let botWithMaxRadius = bots.max(by: { $0.radius < $1.radius })!
        let botsInsideMaxRadius = bots.filter({ $0.coord.manhattan(to: botWithMaxRadius.coord) <= botWithMaxRadius.radius })
        
        print("there are \(botsInsideMaxRadius.count) nanobots in range of the bot with the largest signal")
        
        var xs = bots.map({ $0.coord.x })
        var ys = bots.map({ $0.coord.y })
        var zs = bots.map({ $0.coord.z })
        
        var step = 1
        while step < xs.max()! - xs.min()! {
            step = step * 2
        }
        
        var best: Coord? = nil
        while true {
            var target_count = 0
            for x in stride(from: xs.min()!, to: xs.max()!, by: step) {
                for y in stride(from: ys.min()!, to: ys.max()!, by: step) {
                    for z in stride(from: zs.min()!, to: zs.max()!, by: step) {
                        let current = Coord(x: x, y: y, z: z)
                        var count = 0
                        for bot in bots {
                            let distance = current.manhattan(to: bot.coord)
                            if (distance - bot.radius) / step <= 0 {
                                count = count + 1
                            }
                        }
                        if count > target_count {
                            target_count = count
                            best = current
                        } else if (count == target_count && Coord.zero.manhattan(to: current) < Coord.zero.manhattan(to: best ?? (current + Coord(x: 1, y: 1, z: 1)))) {
                            best = current
                        }
                    }
                }
            }
            
            if step == 1 {
                break
            } else {
                xs = [best!.x - step, best!.x + step]
                ys = [best!.y - step, best!.y + step]
                zs = [best!.z - step, best!.z + step]
                step = step / 2
            }
        }
        
        print("\(best) is in range of the most bots")
        print("distance to it = \(Coord.zero.manhattan(to: best!))")
    }
}
