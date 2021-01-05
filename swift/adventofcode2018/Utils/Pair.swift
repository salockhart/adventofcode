//
//  Pair.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 10/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

public struct Pair<T: Hashable, S: Hashable>: Hashable {
    let left: T
    let right: S
}
