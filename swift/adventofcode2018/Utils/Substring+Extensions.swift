//
//  Substring+Extensions.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 02/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

extension Substring {
    subscript(offset: Int) -> Element {
        return self[index(startIndex, offsetBy: offset)]
    }
    
    
    func substring(with range: NSRange) -> String {
        return (self as NSString).substring(with: range)
    }
}
