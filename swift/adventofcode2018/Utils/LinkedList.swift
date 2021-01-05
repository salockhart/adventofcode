//
//  LinkedList.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 09/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class CircularLinkedList<T: CustomStringConvertible> {
    public var current: LinkedListNode<T>?
    private var count: Int = 0
    
    public init() {}
    
    public init(from list: CircularLinkedList<T>) {
        for node in list.toList() {
            add(prev: node)
        }
    }
    
    public func rotate(_ i: Int) {
        if i == 0 || count == 0 {
            return
        }
        
        if i > 0 {
            current = current?.next
            rotate(i - 1)
        } else if i < 0 {
            current = current?.prev
            rotate(i + 1)
        }
    }
    
    public func add(next contents: T) {
        let A = self.current
        let B = LinkedListNode(contents: contents)
        let C = self.current?.next
        
        A?.next = B
        B.prev = A ?? B
        B.next = C ?? B
        C?.prev = B
        
        count = count + 1
        if current == nil {
            current = B
        }
    }
    
    public func add(prev contents: T) {
        let A = self.current?.prev
        let B = LinkedListNode(contents: contents)
        let C = self.current
        
        A?.next = B
        B.prev = A ?? B
        B.next = C ?? B
        C?.prev = B
        
        count = count + 1
        if current == nil {
            current = B
        }
    }
    
    public func pop() -> T? {
        let A = self.current?.prev
        let B = self.current
        let C = self.current?.next
        
        A?.next = C
        C?.prev = A
        
        current = C
        
        count = max(count - 1, 0)
        
        return B?.contents
    }
    
    public func toList() -> [T] {
        var items = [T]()
        var node = current
        
        for _ in 0..<count {
            items.append(node!.contents)
            node = node?.next
        }
        
        return items
    }
    
    public func toStrings() -> [String] {
        return toList().map({ $0.description })
    }
}

extension CircularLinkedList: CustomStringConvertible {
    public var description: String {
        return "[\(toStrings().joined(separator: ", "))]"
    }
}

extension CircularLinkedList where T: Equatable {
    public func rotateTo(node: T) {
        while let contents = current?.contents, contents != node {
            current = current?.next
        }
    }
}

public class LinkedListNode<T: CustomStringConvertible> {
    public let contents: T
    var next: LinkedListNode<T>?
    weak var prev: LinkedListNode<T>?
    
    init(contents: T) {
        self.contents = contents
    }
}

extension LinkedListNode: CustomStringConvertible {
    public var description: String {
        return contents.description
    }
}
