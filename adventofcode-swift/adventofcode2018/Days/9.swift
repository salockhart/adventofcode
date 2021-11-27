//
//  9.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 09/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day9: Day {
    public static func run(input: [String]) {
        let regex = try! NSRegularExpression(pattern: "\\d+", options: [])
        let input = input
            .map({ line -> (players: Int, highestMarble: Int) in
                let matches = regex.matches(in: String(line), options: [], range: NSMakeRange(0, line.count))
                
                let players = Int((line as NSString).substring(with: matches[0].range))
                let highestMarble = Int((line as NSString).substring(with: matches[1].range))
                return (players ?? 0, highestMarble ?? 0)
            })
            .first!
        
        let list = CircularLinkedList<Int>()
        list.add(next: 0)
        let highestMarble = input.highestMarble
        let numPlayers = input.players
        var scores = [Int:Int]()
        for marble in 1...(highestMarble * 100) {
            let player = (marble - 1) % numPlayers + 1
            
            if marble % 23 == 0 {
                list.rotate(-7)
                let removed = (list.pop() ?? 0)
                scores[player] = (scores[player] ?? 0) + marble + removed
            } else {
                list.rotate(1)
                list.add(next: marble)
                list.rotate(1)
            }
            
            if marble == highestMarble {
                let highScore = scores.max(by: { $0.value < $1.value })
                print("high score = \(highScore?.value ?? 0)")
            }
        }
        
        let highScore = scores.max(by: { $0.value < $1.value })
        print("x100 high score = \(highScore?.value ?? 0)")
    }
}
