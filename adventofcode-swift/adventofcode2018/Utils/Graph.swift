//
//  Graph.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 10/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public struct Graph {
    var nodes: [String:GraphNode]
    
    init() {
        self.nodes = [String:GraphNode]()
    }
}

public class GraphNode {
    let name: String
    var taken: Bool
    var connections: [Edge]
    
    init(name: String) {
        self.name = name
        self.taken = false
        self.connections = [Edge]()
    }
}

public struct Edge {
    let from: GraphNode
    let to: GraphNode
}
