import fileinput
import itertools
import sys
from collections import defaultdict


def add(a, b):
    return (a[0] + b[0], a[1] + b[1], a[2] + b[2])


def subtract(a, b):
    return add(a, (-b[0], -b[1], -b[2]))


def energy(a):
    return abs(a[0]) + abs(a[1]) + abs(a[2])


def total_energy(moons):
    return sum([energy(key) * energy(moons[key]) for key in moons])


def parse(lines):
    moons = {}

    for line in lines:
        line = line.strip().translate({ord(c): None for c in "<>xyz="}).split(", ")
        moons[tuple([int(x) for x in line])] = (0, 0, 0)

    return moons


def move(moons):
    for a, b in itertools.combinations(moons.keys(), 2):
        for c in range(len(a)):
            diff = [1 if i == c else 0 for i in range(len(a))]
            if a[c] < b[c]:
                moons[a] = add(moons[a], tuple(diff))
                moons[b] = subtract(moons[b], tuple(diff))
            elif a[c] > b[c]:
                moons[a] = subtract(moons[a], tuple(diff))
                moons[b] = add(moons[b], tuple(diff))

    return {add(x, moons[x]): moons[x] for x in moons}


def part1(lines):
    moons = parse(lines)

    for _ in range(1000):
        moons = move(moons)

    return total_energy(moons)


def get_axis(moons, idx):
    return tuple([moon[idx] for moon in moons] + [moons[k][idx] for k in moons])


def gcd(x, y):
    while y:
        x, y = y, x % y
    return x


def part2(lines):
    moons = parse(lines)

    initial_axis = [get_axis(moons, idx) for idx in range(3)]
    periods = [0 for _ in range(3)]

    for i in range(sys.maxsize):
        moons = move(moons)

        slices = [get_axis(moons, idx) for idx in range(3)]

        for idx, s in enumerate(slices):
            if s == initial_axis[idx]:
                periods[idx] = i + 1

        if 0 not in periods:
            break

    x, y, z = periods

    lcm2 = y * z // gcd(y, z)
    return x * lcm2 // gcd(x, lcm2)


parts = (part1, part2)


if __name__ == "__main__":
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
