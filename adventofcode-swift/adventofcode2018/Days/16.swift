//
//  16.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 16/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

typealias ExampleOperation = (before: Registers, after: Registers, opcode: Int, instructions: Instructions)
typealias Operation = (opcode: Int, instructions: Instructions)

public class Day16: Day {
    public static func run(input: [String]) {
        let opcodes = [
            Opcodes.addr,
            Opcodes.addi,
            Opcodes.mulr,
            Opcodes.muli,
            Opcodes.banr,
            Opcodes.bani,
            Opcodes.borr,
            Opcodes.bori,
            Opcodes.setr,
            Opcodes.seti,
            Opcodes.gtir,
            Opcodes.gtri,
            Opcodes.gtrr,
            Opcodes.eqir,
            Opcodes.eqri,
            Opcodes.eqrr
        ]
        
        func parseLine(_ input: String) -> [Int] {
            return input
                .split(whereSeparator: { "[, ]".contains($0) })
                .compactMap({ Int($0) })
        }
        
        func parseOperation(_ input: String) -> Operation {
            let instructions = parseLine(input)
            return (opcode: instructions[0], instructions: (A: instructions[1], B: instructions[2], C: instructions[3]))
        }
        
        func parseTriple(_ input: [String]) -> ExampleOperation {
            let before = parseLine(input[0])
            let operation = parseOperation(input[1])
            let after = parseLine(input[2])

            return (before: before, after: after, opcode: operation.opcode, instructions: operation.instructions)
        }
        
        var testData = [ExampleOperation]()
        let inputLines = Array(input.enumerated())
        let grouped = Dictionary(grouping: inputLines, by: { $0.offset / 3 })
            .mapValues({ $0.map({ $0.element }) })
            
        let testGrouping = grouped
            .filter({ $0.value[0].range(of: "Before") != nil })
            .mapValues({ parseTriple($0) })
        
        for i in 0..<testGrouping.count {
            testData.append(testGrouping[i]!)
        }
        
        var behavesLikeThreeOrMore = 0
        var opcodeMapping = [Int:[Int]]()
        for test in testData {
            let results = opcodes
                .map({ $0(test.instructions, test.before) })
            
            var possibles = [Int]()
            for (idx, result) in results.enumerated() {
                if result == test.after {
                    possibles.append(idx)
                }
            }
            
            if (opcodeMapping[test.opcode]?.count ?? Int.max) > possibles.count {
                opcodeMapping[test.opcode] = possibles
            }
            
            if possibles.count >= 3 {
                behavesLikeThreeOrMore += 1
            }
        }
        
        print("\(behavesLikeThreeOrMore) example(s) behave(s) like three or more opcodes")
        
        var handled = [Int]()
        while opcodeMapping.contains(where: { $0.value.count > 1 }) {
            guard let knownMapping = opcodeMapping.first(where: { $0.value.count == 1 && !handled.contains($0.value[0]) }) else {
                fatalError()
            }
            opcodeMapping = opcodeMapping.mapValues({ $0.count == 1 ? $0 : $0.filter({ $0 != knownMapping.value[0] }) })
            handled.append(knownMapping.value[0])
        }
        
        let finalMapping = opcodeMapping.mapValues({ $0[0] })
        
        var program = [Operation]()
        let programGroups = grouped
            .filter({ $0.value[0].range(of: "Before") == nil })
        for i in testData.count..<(programGroups.count + testData.count) {
            program.append(contentsOf: programGroups[i]!.map({ parseOperation($0) }))
        }
        
        var registers: Registers = [0, 0, 0, 0]
        for operation in program {
            registers = opcodes[finalMapping[operation.opcode]!](operation.instructions, registers)
        }
        
        print("After execution, registers = \(registers)")
    }
}
