//
//  21.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 21/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day21: Day {
    static func execute(registers: Registers, instructionPointer: Int, instructions: [(opcode: String, instructions: Instructions)]) -> Registers {
        var currentInstruction = 0
        var registers = registers
        
        var firstR4 = false
        var r4Values = [Int]()
        var lastR4 = 0
        
        while currentInstruction >= 0 && currentInstruction < instructions.count {
            if currentInstruction == 29 {
                if !firstR4 {
                    print("the lowest non-negative integer value for register 0 that causes the program to halt after executing the fewest instructions =", registers[4])
                    firstR4 = true
                }
                if registers[4] != lastR4 {
//                    print(r4Values)
                    if r4Values.contains(registers[4]) {
                        print("the lowest non-negative integer value for register 0 that causes the program to halt after executing the most instructions =", lastR4)
                        break
                    }
                    r4Values.append(registers[4])
                    lastR4 = registers[4]
                }
            }
            
            registers[instructionPointer] = currentInstruction
            let instruction = instructions[currentInstruction]
            registers = opcodes[instruction.opcode]!(instruction.instructions, registers)
            currentInstruction = registers[instructionPointer] + 1
        }
        
        return registers
    }
    
    public static func run(input: [String]) {
        let instructionPointer = Int(String(input[0][String.Index(encodedOffset: 4)]))!
        let instructions = input[1..<input.count].map { instruction -> (opcode: String, instructions: Instructions) in
            let opcode = String(instruction[String.Index(encodedOffset: 0)..<String.Index(encodedOffset: 4)])
            let nums = String(instruction[String.Index(encodedOffset: 5)..<String.Index(encodedOffset: instruction.count)])
                .split(separator: " ")
                .map({ Int(String($0))! })
            let A = nums[0]
            let B = nums[1]
            let C = nums[2]
            
            return (opcode: opcode, instructions: (A: A, B: B, C: C))
        }
        
        execute(registers: [0, 0, 0, 0, 0, 0], instructionPointer: instructionPointer, instructions: instructions)
    }
}
