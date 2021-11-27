//
//  14.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 14/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day14: Day {
    static func execute(until complete: ([Int]) -> Bool) -> [Int] {
        var scoreboard = [3, 7]
        
        var firstElfPosition = 0
        var secondElfPosition = 1
        
        while !complete(scoreboard) {
            let combined = scoreboard[firstElfPosition] + scoreboard[secondElfPosition]
            if combined < 10 {
                scoreboard.append(combined)
            } else {
                scoreboard.append(combined / 10)
                scoreboard.append(combined % 10)
            }
            
            firstElfPosition = (firstElfPosition + scoreboard[firstElfPosition] + 1) % scoreboard.count
            secondElfPosition = (secondElfPosition + scoreboard[secondElfPosition] + 1) % scoreboard.count
        }
        
        return scoreboard
    }
    
    static func part1(numRecipes: Int) {
        let scoreboard = execute { (scoreboard) -> Bool in
            return scoreboard.count > numRecipes + 10
        }
        
        let nextTen = scoreboard[numRecipes..<(numRecipes + 10)]
        print("After \(numRecipes) recipes, the next 10 scores are \(Int(nextTen.map({ String($0) }).joined())!)")
    }
    
    static func part2(search searchingFor: String) {
        let scoreboard = execute { (scoreboard) -> Bool in
            return scoreboard.suffix(searchingFor.count).map({ String($0) }).joined() == searchingFor
                || scoreboard.dropLast().suffix(searchingFor.count).map({ String($0) }).joined() == searchingFor
        }
        
        let numToRemove = String(scoreboard.last!) == String(searchingFor.last!) ? searchingFor.count : searchingFor.count + 1
        
        let scoresList = scoreboard[0..<(scoreboard.count - numToRemove)]
        print("\(searchingFor) first appears after \(scoresList.count) recipes")
    }
    
    public static func run(input: [String]) {
        part1(numRecipes: Int(input.first!)!)
        part2(search: input.first!)
    }
}
