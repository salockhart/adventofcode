//
//  main.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 10/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

func read(caseNumber: String) -> [String] {
    if let contentData = try? Input.get("\(caseNumber).input") {
        return contentData.split(separator: "\n").map({ String($0) })
    }
    
    return []
}

//Day1.run(input: read(caseNumber: "1"))
//Day2.run(input: read(caseNumber: "2"))
//Day3.run(input: read(caseNumber: "3"))
//Day4.run(input: read(caseNumber: "4"))
//Day5.run(input: read(caseNumber: "5"))
//Day6.run(input: read(caseNumber: "6"))
//Day7.run(input: read(caseNumber: "7"))
//Day8.run(input: read(caseNumber: "8"))
//Day9.run(input: read(caseNumber: "9"))
//Day10.run(input: read(caseNumber: "10"))
//Day11.run(input: read(caseNumber: "11"))
//Day12.run(input: read(caseNumber: "12"))
//Day13.run(input: read(caseNumber: "13"))
//Day14.run(input: read(caseNumber: "14"))
//Day15.run(input: read(caseNumber: "15"))
//Day16.run(input: read(caseNumber: "16"))
//Day17.run(input: read(caseNumber: "17"))
//Day18.run(input: read(caseNumber: "18"))
//Day19.run(input: read(caseNumber: "19"))
//Day20.run(input: read(caseNumber: "20"))
//Day21.run(input: read(caseNumber: "21"))
//Day22.run(input: read(caseNumber: "22"))
//Day23.run(input: read(caseNumber: "23"))
//Day24.run(input: read(caseNumber: "24"))
Day25.run(input: read(caseNumber: "25"))
