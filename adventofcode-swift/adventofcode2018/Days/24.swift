//
//  24.swift
//  advent-of-code-2018
//
//  Created by Lockhart, Alex on 24/12/2018.
//  Copyright © 2018 Lockhart, Alex. All rights reserved.
//

import Foundation

extension Sorting {
    static func targetOrder(_ a: Day24.Army, _ b: Day24.Army) -> Bool {
        return a.effectivePower > b.effectivePower || (a.effectivePower == b.effectivePower && a.initiative > b.initiative)
    }
    
    static func initiativeOrder(_ a: Day24.Army, _ b: Day24.Army) -> Bool {
        return a.initiative > b.initiative
    }
}

public class Day24: Day {
    enum ArmyType {
        case immune
        case infection
    }
    
    enum AttackType: String {
        case radiation = "radiation"
        case cold = "cold"
        case fire = "fire"
        case bludgeoning = "bludgeoning"
        case slashing = "slashing"
    }
    
    class Army: Hashable {
        let id: Int
        let type: ArmyType
        private let originalUnits: Int
        private(set) var units: Int
        let health: Int
        let immune: [AttackType]
        let weak: [AttackType]
        let attack: Int
        let attackType: AttackType
        let initiative: Int
        
        private(set) var boost: Int = 0
        
        var dead: Bool {
            return units <= 0
        }
        
        var effectivePower: Int {
            return units * (attack + boost)
        }
        
        init(id: Int, type: ArmyType, units: Int, health: Int, immune: [AttackType], weak: [AttackType], attack: Int, attackType: AttackType, initiative: Int) {
            self.id = id
            self.type = type
            self.originalUnits = units
            self.units = units
            self.health = health
            self.immune = immune
            self.weak = weak
            self.attack = attack
            self.attackType = attackType
            self.initiative = initiative
        }
        
        func damage(against: Army) -> Int {
            if against.immune.contains(attackType) {
                return 0
            } else if against.weak.contains(attackType) {
                return effectivePower * 2
            } else {
                return effectivePower
            }
        }
        
        func hit(for dmg: Int) {
            let unitsDown = dmg / health
            units = max(0, units - unitsDown)
        }
        
        func reset(with boost: Int) {
            self.units = self.originalUnits
            self.boost = boost
        }
        
        func hash(into hasher: inout Hasher) {
            hasher.combine(id)
            hasher.combine(type)
        }
        
        static func == (lhs: Day24.Army, rhs: Day24.Army) -> Bool {
            return lhs.id == rhs.id && lhs.type == rhs.type
        }
    }
    
    struct Attack: Hashable {
        let from: Army
        let to: Army
    }
    
    public static func run(input: [String]) {
        var immune = [Army]()
        var infection = [Army]()
        var current = ArmyType.immune
        for (id, line) in input.enumerated() {
            if line == "Immune System:" {
                current = .immune
            } else if line == "Infection:" {
                current = .infection
            } else {
                let regex = try! NSRegularExpression(pattern: "^(\\d+) units each with (\\d+) hit points (\\([^)]*\\) )?with an attack that does (\\d+) (.*?) damage at initiative (\\d+)$", options: [])
                
                let army = regex
                    .matches(in: line, options: [], range: NSRange(location: 0, length: line.count))
                    .map({ matches -> Army in
                        let units = Int((line as NSString).substring(with: matches.range(at: 1)))!
                        let health = Int((line as NSString).substring(with: matches.range(at: 2)))!
                        let attack = Int((line as NSString).substring(with: matches.range(at: 4)))!
                        let attackType = AttackType(rawValue: (line as NSString).substring(with: matches.range(at: 5)))!
                        let initiative = Int((line as NSString).substring(with: matches.range(at: 6)))!
                        
                        var weak = [AttackType]()
                        var immune = [AttackType]()
                        
                        if matches.range(at: 3).length != 0 {
                            for piece in (line as NSString).substring(with: matches.range(at: 3)).split(whereSeparator: { "(;)".contains($0) }) {
                                let types = piece
                                    .split(whereSeparator: { ", ".contains($0) })
                                    .map({ String($0) })
                                    .compactMap({ AttackType(rawValue: $0) })
                                
                                if piece.trimmingCharacters(in: .whitespaces).starts(with: "immune to") {
                                    immune.append(contentsOf: types)
                                } else {
                                    weak.append(contentsOf: types)
                                }
                            }
                        }
                        
                        return Army(id: id, type: current, units: units, health: health, immune: immune, weak: weak, attack: attack, attackType: attackType, initiative: initiative)
                    })
                    .first!
                
                if current == .immune {
                    immune.append(army)
                } else {
                    infection.append(army)
                }
            }
        }
        
        func statusReport() {
            print("Immune System:")
            print(immune.contains(where: { !$0.dead }) ? immune.map({ "Group \($0.id + 1) contains \($0.units) units" }).joined(separator: "\n") : "No groups remain.")
            print("Infection:")
            print(infection.contains(where: { !$0.dead }) ? infection.map({ "Group \($0.id + 1) contains \($0.units) units" }).joined(separator: "\n") : "No groups remain.")
            print()
        }
        
        func livingArmies() -> [Army] {
            return (immune + infection).filter({ !$0.dead })
        }
        
        for boost in 0...1000 {
            for army in immune {
                army.reset(with: boost)
            }
            
            for army in infection {
                army.reset(with: 0)
            }
            
            var didDamage = false
            repeat {
//                statusReport()
//                print()
                
                var plannedAttacks = [Attack]()
                for army in livingArmies().sorted(by: Sorting.targetOrder) {
                    let otherArmies = (army.type == .immune ? infection : immune).filter({ !$0.dead && !plannedAttacks.map({ $0.to }).contains($0) })
                        .map({ otherArmy -> (Army, Int) in
                            let damage = army.damage(against: otherArmy)
                             return (otherArmy, damage)
                        })
                    
                    let maxDamage = otherArmies.map({ $0.1 }).max() ?? 0
                    if maxDamage != 0, let targetArmy = otherArmies.filter({ $0.1 == maxDamage }).sorted(by: { Sorting.targetOrder($0.0, $1.0) }).first {
                        plannedAttacks.append(Attack(from: army, to: targetArmy.0))
                    }
                }
                
                didDamage = false
                for army in (immune + infection).sorted(by: Sorting.initiativeOrder) {
                    if army.dead {
                        continue
                    }
                    if let attack = plannedAttacks.first(where: { $0.from == army }) {
                        let unitsBefore = attack.to.units
                        attack.to.hit(for: attack.from.damage(against: attack.to))
                        if unitsBefore - attack.to.units > 0 {
                            didDamage = true
                        }
                    }
                }
            } while immune.contains(where: { !$0.dead }) && infection.contains(where: { !$0.dead }) && didDamage
            
            if immune.allSatisfy({ $0.dead }) || infection.allSatisfy({ $0.dead }) {
//                statusReport()
                
                let winner = immune.allSatisfy({ $0.dead }) ? "infection" : "immune system"
                
                print("for boost of \(boost), \(winner) wins and they have \(livingArmies().map({ $0.units }).reduce(0, +)) units remaining")
                
                if winner == "immune system" {
                    break
                }
            } else {
                print("for boost of \(boost), deadlock!")
            }
        }
    }
}
