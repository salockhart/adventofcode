//
//  19.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 19/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

let opcodes = [
    "addr": Opcodes.addr,
    "addi": Opcodes.addi,
    "mulr": Opcodes.mulr,
    "muli": Opcodes.muli,
    "banr": Opcodes.banr,
    "bani": Opcodes.bani,
    "borr": Opcodes.borr,
    "bori": Opcodes.bori,
    "setr": Opcodes.setr,
    "seti": Opcodes.seti,
    "gtir": Opcodes.gtir,
    "gtri": Opcodes.gtri,
    "gtrr": Opcodes.gtrr,
    "eqir": Opcodes.eqir,
    "eqri": Opcodes.eqri,
    "eqrr": Opcodes.eqrr
]

public class Day19: Day {
    static func execute(registers: Registers, instructionPointer: Int, instructions: [(opcode: String, instructions: Instructions)]) -> Registers {
        var currentInstruction = 0
        var registers = registers
        
        while currentInstruction >= 0 && currentInstruction < instructions.count {
            if currentInstruction == 1 {
                print(registers)
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
        
        print("register @ 0 == 0, register 0 after execution = \(execute(registers: [0, 0, 0, 0, 0, 0], instructionPointer: instructionPointer, instructions: instructions)[0])")
//        print("register @ 0 == 1, registers after execution = \(execute(registers: [1, 0, 0, 0, 0, 0], instructionPointer: instructionPointer, instructions: instructions)[0])")
        
//        That runs for a while. Instead, the code ends up looking something like:
//        let factoring = 10551425
//        var result = 0
//
//        for i in 1...factoring {
//            for j in 1...factoring {
//                if i * j == factoring {
//                    result = result + i
//                }
//            }
//        }
//        which sums up the factors of the calculated value
//        More efficiently, we can do:
        let factoring = 10551425
        var factors = [Int]()
        
        let sqrtn = Int(Double(factoring).squareRoot())
        factors.reserveCapacity(sqrtn * 2)
        for i in 1...sqrtn {
            if factoring % i == 0 {
                factors.append(i)
                factors.append(factoring / i)
            }
        }
        
        print("register @ 0 == 1, register 0 after execution = \(factors.reduce(0, +))")
    }
}
