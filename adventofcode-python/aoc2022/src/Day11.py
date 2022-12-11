from functools import reduce

from aocd import data, submit


def parse_monkey(input):
    lines = input.splitlines()

    id = int(lines.pop(0).strip()[len("Monkey ") : -1])

    starting_items = [
        int(x)
        for x in lines.pop(0).strip()[len("Starting items: ") :].split(", ")
        if x != ""
    ]

    operation = None
    match lines.pop(0).strip()[len("Operation: ") :].split(" "):
        case ["new", "=", "old", "*", "old"]:
            operation = lambda x: x * x
        case ["new", "=", "old", "*", num]:
            operation = lambda x: x * int(num)
        case ["new", "=", "old", "+", num]:
            operation = lambda x: x + int(num)

    divisor = int(lines.pop(0).strip()[len("Test: divisible by ") :])
    true_target = int(lines.pop(0).strip()[len("If true: throw to monkey ") :])
    false_target = int(lines.pop(0).strip()[len("If false: throw to monkey ") :])
    test = lambda x: true_target if x % divisor == 0 else false_target

    monkey = {
        "id": id,
        "items": starting_items,
        "operation": operation,
        "divisor": divisor,
        "test": test,
    }

    return monkey


def parse(input):
    return [parse_monkey(x) for x in input.split("\n\n")]


def solve(monkeys, rounds, manage_worry):
    inspections = {}

    for _ in range(0, rounds):
        for monkey in monkeys:
            for item in monkey["items"]:
                new_item = monkey["operation"](item)

                if monkey["id"] not in inspections:
                    inspections[monkey["id"]] = 0
                inspections[monkey["id"]] += 1

                new_item = manage_worry(new_item)

                dest = monkey["test"](new_item)
                monkeys[dest]["items"].append(new_item)
            monkey["items"] = []

    print(inspections)

    [a, b] = sorted(inspections.values(), reverse=True)[0:2]
    return a * b


def part1(input):
    monkeys = parse(input)

    return solve(monkeys, 20, lambda x: x // 3)


def part2(input):
    monkeys = parse(input)
    lcm = reduce(lambda acc, x: acc * x, [monkey["divisor"] for monkey in monkeys], 1)

    return solve(monkeys, 10000, lambda x: x % lcm)


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
