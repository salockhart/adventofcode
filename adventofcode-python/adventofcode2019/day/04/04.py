import fileinput


def parse(lines):
    [lower, upper] = [line.strip().split("-") for line in lines][0]
    return [int(lower), int(upper)]


def getParts(i):
    parts = []
    while i > 0:
        parts.insert(0, i % 10)
        i = i // 10
    return parts


def meetsCriteria(parts):
    foundDouble = False
    for idx, x in enumerate(parts):
        if min(parts[idx:]) < x:
            return False

        if parts.count(x) >= 2:
            foundDouble = True

    return foundDouble


def part1(lines):
    [lower, upper] = parse(lines)

    return len(filter(meetsCriteria, map(getParts, range(lower, upper))))


def meetsStrictCriteria(parts):
    foundDouble = False

    for idx, x in enumerate(parts):
        if min(parts[idx:]) < x:
            return False

        if parts.count(x) == 2:
            foundDouble = True

    return foundDouble


def part2(lines):
    [lower, upper] = parse(lines)

    return len(filter(meetsStrictCriteria, map(getParts, range(lower, upper))))


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
