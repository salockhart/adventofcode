//
//  7.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 07/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Day7: Day {
    public static func run(input: [String]) {
        let edges = input
            .map { String($0) }
            .map { line -> Edge in
                let from = String(line[String.Index(encodedOffset: 5)])
                let to = String(line[String.Index(encodedOffset: 36)])
                
                return Edge(from: GraphNode(name: from), to: GraphNode(name: to))
        }
        
        var graph = edges
            .reduce(Graph()) { (graph, edge) -> Graph in
                var graph = graph
                if graph.nodes[edge.from.name] == nil {
                    graph.nodes[edge.from.name] = edge.from
                }
                
                if graph.nodes[edge.to.name] == nil {
                    graph.nodes[edge.to.name] = edge.to
                }
                
                graph.nodes[edge.to.name]?.connections.append(edge)
                
                return graph
        }
        
        func time(for node: String) -> Int {
            return Int(node.unicodeScalars.first!.value) - 64
        }
        
        var second = -1
        var workers = [(node: GraphNode?, remaining: Int)](repeating: (node: nil, remaining: -1), count: 5)
        var order = [String]()
        
//        let workerNames = workers.enumerated().map({ "\($0.offset)" }).reduce("") { $0 + "Worker " + $1 + "\t" }
//        print("Second\t\(workerNames)\tDone")
        
        while (graph.nodes.count > 0 || workers.contains(where: { $0.node != nil })) {
            second = second + 1
            
            workers = workers.enumerated().map({ pair in
                let (idx, job) = pair
                if job.remaining == 0 {
                    let node = job.node!
                    graph.nodes.removeValue(forKey: node.name)
                    graph.nodes = graph.nodes.mapValues({ (n) -> GraphNode in
                        n.connections = n.connections.filter { $0.from.name != node.name }
                        return n
                    })
                    order.append(node.name)
                    return (node: nil, remaining: -1)
                }
                return job
            })
            
            var ready = graph.nodes
                .filter { (pair) -> Bool in
                    return pair.value.connections.count == 0 && !pair.value.taken
                }
                .sorted(by: { (a, b) -> Bool in
                    return a.key.compare(b.key) == .orderedAscending
                })
            
            workers = workers.enumerated().map({ pair in
                var (idx, job) = pair
                if job.node == nil && ready.count < 1 {
                    return job
                }
                
                func getGraphNode() -> GraphNode {
                    let next = ready.removeFirst()
                    graph.nodes[next.key]?.taken = true
                    job.node = next.value
                    job.remaining = 60 + time(for: next.key)
                    return next.value
                }
                
                let node = job.node ?? getGraphNode()
                
                var remaining = job.remaining
                remaining = remaining - 1
                
                return (node: node, remaining: remaining)
            })
            
//            let progress = workers.map({ $0.node?.name ?? "." }).reduce("", { $0 + $1 + "\t\t\t"})
//
//            print("\(second)\t\t\(progress)\t\(order.joined())")
        }
        
        print("order completed = \(order.joined())")
        print("seconds taken = \(second)")
    }
}
