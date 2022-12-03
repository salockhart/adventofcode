from aocd import data, submit


def split(input):
    return (input[0 : (len(input) // 2)], input[(len(input) // 2) : len(input)])


def intersect(pair):
    return [v for v in pair[0] if v in pair[1]]


def priority(x):
    ordinal = ord(x)
    if ordinal > 96:
        return ordinal - 96
    else:
        return ordinal - 38


def part1(input):
    return sum(
        [priority(x) for x in [intersect(split(r))[0] for r in input.splitlines()]]
    )


def part2(input):
    rs = [r for r in input.splitlines()]
    num_groups = len(rs) // 3
    badges = []
    while len(rs) > 0:
        group = rs[:3]
        rs = rs[3:]
        badges.append(intersect((intersect((group[0], group[1])), group[2]))[0])
    return sum([priority(badge) for badge in badges])


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
