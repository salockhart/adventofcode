from aocd import data, submit

combos1 = {
    ("A", "X"): 1 + 3,
    ("A", "Y"): 2 + 6,
    ("A", "Z"): 3 + 0,
    ("B", "X"): 1 + 0,
    ("B", "Y"): 2 + 3,
    ("B", "Z"): 3 + 6,
    ("C", "X"): 1 + 6,
    ("C", "Y"): 2 + 0,
    ("C", "Z"): 3 + 3,
}

combos2 = {
    ("A", "X"): 3 + 0,
    ("A", "Y"): 1 + 3,
    ("A", "Z"): 2 + 6,
    ("B", "X"): 1 + 0,
    ("B", "Y"): 2 + 3,
    ("B", "Z"): 3 + 6,
    ("C", "X"): 2 + 0,
    ("C", "Y"): 3 + 3,
    ("C", "Z"): 1 + 6,
}


def part1(input):
    rounds = input.splitlines()
    scores = [combos1[tuple(round.split())] for round in rounds]
    return sum(scores)


def part2(input):
    rounds = input.splitlines()
    scores = [combos2[tuple(round.split())] for round in rounds]
    return sum(scores)


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
