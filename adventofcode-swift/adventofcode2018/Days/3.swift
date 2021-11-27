//
//  3.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 03/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

struct Claim {
    let id: Int
    let x: Int
    let y: Int
    let width: Int
    let height: Int
}

public class Day3: Day {
    public static func run(input: [String]) {
        let regex = try? NSRegularExpression(pattern: "[0-9]+")
        let claims = input
            .map { line -> Claim in
                let matches = regex!
                    .matches(in: line, options: [], range: NSRange(location: 0, length: line.count))
                    .map { match in
                        return (line as NSString).substring(with: match.range)
                    }
                    .map { Int($0) ?? 0 }
                
                return Claim(id: matches[0], x: matches[1], y: matches[2], width: matches[3], height: matches[4])
            }
        
        var claimIDs = claims.map { $0.id }.reduce([Int:Bool?]()) { dict, id in
            var dict = dict
            dict[id] = nil
            return dict
        }
        
        var fabric = Array(repeating: Array(repeating: [Int](), count: 1000), count: 1000)
        
        for claim in claims {
            for x in claim.x..<(claim.x + claim.width) {
                for y in claim.y..<(claim.y + claim.height) {
                    fabric[x][y].append(claim.id)
                    
                    if fabric[x][y].count == 1 && claimIDs[claim.id] == nil {
                        claimIDs[claim.id] = true
                    } else if fabric[x][y].count > 1 {
                        for id in fabric[x][y] {
                            claimIDs[id] = false
                        }
                    }
                }
            }
        }
        
        let count = fabric
            .flatMap { $0 }
            .filter { $0.count >= 2 }
            .count
        
        print(count)
        
        print(claimIDs.filter { $0.value ?? false })
    }
}
