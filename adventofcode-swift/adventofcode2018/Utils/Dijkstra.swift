//
//  Dijkstra.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 15/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public func dijkstra<T: Hashable>(from: T,
                                  to: T? = nil,
                                  count: Int,
                                  neighbours: (T) -> [T],
                                  tiebreaker: (([T], [T]) -> Bool)? = nil,
                                  weight: (T, T) -> Int = { _, _ in 1 }) -> [T:(distance: Int, path: [T])] {
    
    var findings = [T:(distance: Int, path: [T])]()
    var visited = [T]()

    findings[from] = (distance: 0, path: [from])

    var current: T? = from

    while current != to, let vertex = current {
        visited.append(vertex)
        let newNeighbours = neighbours(vertex).filter({ !visited.contains($0) })

        for neighbour in newNeighbours {
            let theoreticNewWeight = (findings[vertex]?.distance ?? 0) + weight(vertex, neighbour)
            let theoreticNewPath = (findings[vertex]?.path ?? []) + [neighbour]
            
            if theoreticNewWeight < (findings[neighbour]?.distance ?? Int.max)
                || (theoreticNewWeight == (findings[neighbour]?.distance ?? Int.max) && (tiebreaker?(theoreticNewPath, findings[neighbour]?.path ?? []) ?? false)) {
                findings[neighbour] = (distance: theoreticNewWeight, path: theoreticNewPath)
            }
        }

        if visited.count == count {
            current = nil
        } else {
            current = findings
                .filter({ !visited.contains($0.key) })
                .min(by: { $0.value.distance < $1.value.distance })?
                .key
        }
    }

    return findings
}
