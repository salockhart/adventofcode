from math import sqrt

from aocd import data, submit


def sum_coords(a, b):
    return (a[0] + b[0], a[1] + b[1])


def clamp(a):
    return max(-1, min(a, 1))


def parse(input):
    return [x for line in input.splitlines() for x in line[0] * int(line[2:])]


def dist2D(a, b):
    return sqrt(((a[0] - b[0]) ** 2) + ((a[1] - b[1]) ** 2))


def simulate(rope, steps):
    tail_hist: set[tuple[int, int]] = set([(0, 0)])

    for step in steps:
        match step:
            case "U":
                rope[0] = sum_coords(rope[0], (0, 1))
            case "D":
                rope[0] = sum_coords(rope[0], (0, -1))
            case "L":
                rope[0] = sum_coords(rope[0], (-1, 0))
            case "R":
                rope[0] = sum_coords(rope[0], (1, 0))

        prev_knot = rope[0]
        for idx, knot in enumerate(rope):
            if idx == 0:
                continue

            dist = dist2D(prev_knot, knot)

            if dist >= 2:
                diff = (clamp(prev_knot[0] - knot[0]), clamp(prev_knot[1] - knot[1]))
                rope[idx] = sum_coords(rope[idx], diff)

            if idx == (len(rope) - 1):
                tail_hist.add(rope[idx])

            prev_knot = rope[idx]

    return len(tail_hist)


def part1(input):
    rope: list[tuple[int, int]] = [(0, 0), (0, 0)]
    steps = parse(input)

    return simulate(rope, steps)


def part2(input):
    rope: list[tuple[int, int]] = [
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
    ]
    steps = parse(input)

    return simulate(rope, steps)


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
