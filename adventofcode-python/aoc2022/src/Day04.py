from aocd import data, submit


def part1(input):
    pairs = [x for x in input.splitlines()]
    fully_contained = 0
    for pair in pairs:
        [left, right] = pair.split(",")
        [leftMin, leftMax] = [int(x) for x in left.split("-")]
        [rightMin, rightMax] = [int(x) for x in right.split("-")]

        if leftMin <= rightMin and leftMax >= rightMax:
            fully_contained += 1
        elif leftMin >= rightMin and leftMax <= rightMax:
            fully_contained += 1
    return fully_contained


def part2(input):
    pairs = [x for x in input.splitlines()]
    overlapping = 0
    for pair in pairs:
        [left, right] = pair.split(",")
        [leftMin, leftMax] = [int(x) for x in left.split("-")]
        [rightMin, rightMax] = [int(x) for x in right.split("-")]

        if leftMin in range(rightMin, rightMax + 1):
            overlapping += 1
        elif leftMax in range(rightMin, rightMax + 1):
            overlapping += 1
        elif rightMin in range(leftMin, leftMax + 1):
            overlapping += 1
        elif rightMax in range(leftMin, leftMax + 1):
            overlapping += 1

    return overlapping


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
