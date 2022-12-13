from functools import cmp_to_key

from aocd import data, submit


def is_in_order(pair, prefix=""):
    (left, right) = pair

    for (l, r) in zip(left, right):
        if isinstance(l, int):
            if isinstance(r, int):
                if l != r:
                    return l < r
            else:
                internal = is_in_order(([l], r), prefix="\t%s" % prefix)
                if internal != None:
                    return internal
        elif isinstance(r, int):
            internal = is_in_order((l, [r]), prefix="\t%s" % prefix)
            if internal != None:
                return internal
        else:
            internal = is_in_order((l, r), prefix="\t%s" % prefix)
            if internal != None:
                return internal

    if len(left) < len(right):
        return True

    if len(left) > len(right):
        return False


def part1(input):
    pairs = [pair.splitlines() for pair in input.split("\n\n")]
    pairs = [(eval(left), eval(right)) for [left, right] in pairs]
    results = list(map(is_in_order, pairs))
    return sum([idx + 1 for idx, correct in enumerate(results) if correct])


def part2(input):
    lines = [eval(line) for line in input.splitlines() if line != ""]
    sortedLines = sorted(
        lines,
        key=cmp_to_key(lambda a, b: -1 if is_in_order((a, b)) else 1),
    )

    divider_one = [[2]]
    divider_two = [[6]]
    idx_one = 1
    idx_two = 1

    for i in range(0, len(sortedLines) - 1):
        left = sortedLines[i]
        right = sortedLines[i + 1]
        if is_in_order((left, divider_one)) and is_in_order((divider_one, right)):
            idx_one = i + 2
        elif is_in_order((left, divider_two)) and is_in_order((divider_two, right)):
            idx_two = i + 2

    return (idx_one + 1) * idx_two if idx_one > idx_two else idx_one * (idx_two + 1)


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
