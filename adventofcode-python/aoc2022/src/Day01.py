from aocd import data, submit


def parse(input):
    return [sum([int(y) for y in x.splitlines()]) for x in input.strip().split("\n\n")]


def part1(input):
    return max(parse(input))


def part2(input):
    return sum(sorted(parse(input), reverse=True)[0:3])


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
