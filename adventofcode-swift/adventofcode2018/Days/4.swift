//
//  4.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 04/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

enum Event {
    case started(guardID: Int)
    case slept(time: Int)
    case woke(time: Int)
    
    init(from line: String) {
        let dateString = line[String.Index(encodedOffset: 1)...String.Index(encodedOffset: 16)]
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "yyyy-MM-dd HH:mm"
        
        let date = dateFormatter.date(from: String(dateString))!
        let minutes = Calendar.current.component(.minute, from: date)
        
        if line.range(of: "wakes up") != nil {
            self = .woke(time: minutes)
        } else if line.range(of: "falls asleep") != nil {
            self = .slept(time: minutes)
        } else {
            self = .started(guardID: Int(line[String.Index(encodedOffset: 26)...String.Index(encodedOffset: 29)]) ?? 0)
        }
    }
}

public class Day4: Day {
    public static func run(input: [String]) {
        var lastGuardID = -1
        var timeAsleep = 0
        var schedule = [Int:Int]()
        var freq = [Pair<Int, Int>:Int]()
        
        for line in input.sorted() {
            let event = Event(from: line)
            
            switch event {
            case .started(let guardID):
                lastGuardID = guardID
            case .slept(let time):
                timeAsleep = time
            case .woke(let time):
                for second in stride(from: timeAsleep, to: time, by: 1) {
                    schedule[lastGuardID] = (schedule[lastGuardID] ?? 0) + 1
                    freq[Pair(left: lastGuardID, right: second)] = (freq[Pair(left: lastGuardID, right: second)] ?? 0) + 1
                }
            }
        }
        
        let sleepiest: Int? = schedule.max(by: { $0.value < $1.value })?.key
        print("sleepiest guard = \(sleepiest ?? 0)")
        
        let bestTime = freq.filter({ $0.key.left == sleepiest }).max(by: { $0.value < $1.value })?.key
        
        print("best time = \(bestTime?.right ?? 0)")
        
        let mostLikely = freq.max(by: { $0.value < $1.value })?.key
        
        print("most likely = \(mostLikely?.right ?? 0)")
    }
}
