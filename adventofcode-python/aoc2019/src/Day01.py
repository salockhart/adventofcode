import fileinput


def fuel(mass):
    return mass // 3 - 2


def fuels(mass):
    if mass <= 0:
        return []

    f = fuel(mass)
    return [f] + fuels(f)


def part1(lines):
    return sum(fuel(int(line)) for line in lines)


def part2(lines):
    return sum(weight for line in lines for weight in fuels(int(line)))


parts = (part1, part2)


if __name__ == "__main__":
    lines = list(fileinput.input())
    print("part 1: %d" % part1(lines))
    print("part 2: %d" % part2(lines))
