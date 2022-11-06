import fileinput
import math
import re
import time
from collections import defaultdict, namedtuple


def parse(lines):
    graph = {}
    for line in lines:
        lhs, rhs = line.strip().split(" => ")
        ins = [tuple(x.split()) for x in lhs.split(", ")]
        outs = tuple(rhs.split())

        outs = (int(outs[0]), outs[1])

        for i, x in enumerate(ins):
            ins[i] = (int(x[0]), x[1])

        graph[outs[1]] = (outs[0], ins)

    return graph


def create(graph, fuel):
    need = defaultdict(int, {"FUEL": fuel})
    have = defaultdict(int)
    while True:
        try:
            nk = next(n for n in need if n != "ORE")
        except StopIteration:
            break

        quant, ins = graph[nk]
        d, m = divmod(need[nk], quant)
        del need[nk]
        if m != 0:
            have[nk] = quant - m
            d += 1

        for a, b in ins:
            need[b] += d * a - have[b]
            del have[b]

    return need["ORE"]


def part1(lines):
    graph = parse(lines)

    return create(graph, 1)


def part2(lines):
    graph = parse(lines)

    ONE_TRILLION = 1000000000000

    ore = 0
    fuel = 5000
    over = False
    while True:
        ore = create(graph, fuel)

        if ore > ONE_TRILLION:
            fuel -= 1
            over = True
        else:
            if over:
                return fuel

            fuel += 5000

    return fuel


parts = (part1, part2)


if __name__ == "__main__":
    lines = list(fileinput.input())

    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
