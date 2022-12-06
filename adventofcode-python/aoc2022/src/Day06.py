from aocd import data, submit


def find_marker(input, target_length):
    last = []
    for i, char in enumerate(input):
        if len(last) < target_length:
            last.append(char)
        elif len(set(last)) != len(last):
            last.append(char)
            last.pop(0)
        else:
            return i


def part1(input):
    return find_marker(input, target_length=4)


def part2(input):
    return find_marker(input, target_length=14)


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
