//
//  Opcodes.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 16/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public typealias Instructions = (A: Int, B: Int, C: Int)
public typealias Registers = [Int]

public class Opcodes {
    private static func operater(operation: (Int, Int) -> Int, instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = operation(registers[instructions.A], registers[instructions.B])
        return registers
    }
    
    private static func operatei(operation: (Int, Int) -> Int, instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = operation(registers[instructions.A], instructions.B)
        return registers
    }
    
    public static func addr(instructions: Instructions, registers: Registers) -> Registers {
        return operater(operation: +, instructions: instructions, registers: registers)
    }
    
    public static func addi(instructions: Instructions, registers: Registers) -> Registers {
        return operatei(operation: +, instructions: instructions, registers: registers)
    }
    
    public static func mulr(instructions: Instructions, registers: Registers) -> Registers {
        return operater(operation: *, instructions: instructions, registers: registers)
    }
    
    public static func muli(instructions: Instructions, registers: Registers) -> Registers {
        return operatei(operation: *, instructions: instructions, registers: registers)
    }
    
    public static func banr(instructions: Instructions, registers: Registers) -> Registers {
        return operater(operation: &, instructions: instructions, registers: registers)
    }
    
    public static func bani(instructions: Instructions, registers: Registers) -> Registers {
        return operatei(operation: &, instructions: instructions, registers: registers)
    }
    
    public static func borr(instructions: Instructions, registers: Registers) -> Registers {
        return operater(operation: |, instructions: instructions, registers: registers)
    }
    
    public static func bori(instructions: Instructions, registers: Registers) -> Registers {
        return operatei(operation: |, instructions: instructions, registers: registers)
    }
    
    public static func setr(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = registers[instructions.A]
        return registers
    }
    
    public static func seti(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = instructions.A
        return registers
    }
    
    public static func gtir(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = instructions.A > registers[instructions.B] ? 1 : 0
        return registers
    }
    
    public static func gtri(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = registers[instructions.A] > instructions.B ? 1 : 0
        return registers
    }
    
    public static func gtrr(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = registers[instructions.A] > registers[instructions.B] ? 1 : 0
        return registers
    }
    
    public static func eqir(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = instructions.A == registers[instructions.B] ? 1 : 0
        return registers
    }
    
    public static func eqri(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = registers[instructions.A] == instructions.B ? 1 : 0
        return registers
    }
    
    public static func eqrr(instructions: Instructions, registers: Registers) -> Registers {
        var registers = registers
        registers[instructions.C] = registers[instructions.A] == registers[instructions.B] ? 1 : 0
        return registers
    }
}
