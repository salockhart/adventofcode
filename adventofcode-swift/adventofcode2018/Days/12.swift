//
//  12.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 12/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day12: Day {
    static func age(state: String, mappings: [String:Character]) -> String {
        var newState = [Character]()
        for i in (-2)..<(state.count + 2) {
            let chunkRange = (i - 2)...(i + 2)
            let chunk = chunkRange.map({ String.Index(encodedOffset: $0) }).map({ $0 >= state.endIndex ? "." : state[$0] }).map({ String($0) }).joined()
            newState.append(mappings[chunk] ?? ".")
        }
        return newState.map({ String($0) }).joined()
    }

    static func score(state: String, firstValue: Int) -> Int {
        return state.enumerated().reduce(0, { $0 + ($1.element == "#" ? $1.offset + firstValue : 0) })
    }

    public static func run(input: [String]) {
        let initialState = String(input[0][String.Index(encodedOffset: 15)...String.Index(encodedOffset: input[0].count - 1)])

        var mappings = [String:Character]()

        for i in 1..<input.count {
            let mapping = input[i]
            let from = mapping[String.Index(encodedOffset: 0)..<String.Index(encodedOffset: 5)]
            let to = mapping[String.Index(encodedOffset: mapping.count - 1)]
            mappings[String(from)] = to
        }

//        print(" \(0): \(initialState)")
        var state = initialState
        var states = [String:(gen: Int, pot: Int)]()
        for gen in 1...50000000000 {
            state = age(state: state, mappings: mappings)

            var start = -2 * gen
            let first = state.firstIndex(of: "#")!.encodedOffset
            let last = state.lastIndex(of: "#")!.encodedOffset
            start = start + first

            let trimmedState = String(state[String.Index(encodedOffset: first)...String.Index(encodedOffset: last)])

            if let prevOccurance = states[trimmedState] {
                let generationsSince = gen - prevOccurance.gen
                let potsSince = start - prevOccurance.pot
                let generationsToGo = 50000000000 - prevOccurance.gen

                let targetGenerationOffset = generationsToGo % generationsSince
                let finalTrimmedState = states.first(where: { $0.value.gen == prevOccurance.gen + targetGenerationOffset })!

                let deltaPot = potsSince * (generationsToGo / generationsSince)
                let finalPot = finalTrimmedState.value.pot + deltaPot

                print("after 50000000000 generations score = \(score(state: finalTrimmedState.key, firstValue: finalPot))")
                break
            }
            states[trimmedState] = (gen: gen, pot: start)

//            print("\(gen < 10 ? " " : "")\(gen): \(state)")
            let val = score(state: trimmedState, firstValue: start)
            if gen == 20 {
                print("after \(gen) generations score = \(val)")
            }
        }
    }
}
