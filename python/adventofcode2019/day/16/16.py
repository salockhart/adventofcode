import fileinput
import math
import re
import time
from collections import defaultdict, namedtuple

PATTERN = [0, 1, 0, -1]


def parse(lines):
    return [int(x) for x in [line for line in lines][0]]


def build_pattern(idx, length):
    pattern = []
    for chunk in [[x] * (idx+1) for x in PATTERN]:
        pattern += chunk
    pattern.append(pattern.pop(0))

    while len(pattern) < length:
        pattern += pattern

    return pattern


def part1(lines):
    phase = parse(lines)

    for _ in range(100):
        new_phase = []
        for i in range(len(phase)):
            pattern = build_pattern(i, len(phase))

            eq = list(zip(phase, pattern))

            new_phase.append(abs(sum([x*y for x, y in eq])) % 10)

        phase = new_phase

    return ''.join(str(x) for x in phase[0:8])


def part2(lines):
    phase = parse(lines) * 10000
    offset = int("".join(map(str, phase[:7])))
    phase = phase[offset:]

    for _ in range(100):
        s = sum(phase)
        new_phase = []
        for i in range(len(phase)):
            new_phase += [((s % 10) + 10) % 10]
            s -= phase[i]
        phase = new_phase

    return ''.join(str(x) for x in phase[0:8])


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())

    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
