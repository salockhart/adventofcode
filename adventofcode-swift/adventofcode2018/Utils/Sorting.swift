//
//  Sorting.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 15/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public class Sorting {
    public static func readingOrder(_ a: Coord, _ b: Coord) -> Bool {
        return a.y < b.y || (a.y == b.y && a.x < b.x)
    }
}
