from aocd import data, submit


def parse(crates):
    rows = [
        [row[x : x + 4][1].strip() for x in range(0, len(row), 4)]
        for row in crates.split("\n")[:-1]
    ]
    return [[row[i] for row in rows if row[i] != ""] for i in range(0, len(rows[0]))]


def part1(input):
    [crates, instructions] = input.split("\n\n")
    cols = parse(crates)

    for instruction in instructions.splitlines():
        [m, num, f, fromCol, t, toCol] = instruction.split(" ")

        for i in range(0, int(num)):
            crate = cols[int(fromCol) - 1].pop(0)
            cols[int(toCol) - 1].insert(0, crate)

    return "".join([col[0] for col in cols])


def part2(input):
    [crates, instructions] = input.split("\n\n")
    cols = parse(crates)

    for instruction in instructions.splitlines():
        [m, num, f, fromCol, t, toCol] = instruction.split(" ")

        moved_crates = cols[int(fromCol) - 1][0 : int(num)]
        cols[int(fromCol) - 1] = cols[int(fromCol) - 1][int(num) :]

        cols[int(toCol) - 1] = moved_crates + cols[int(toCol) - 1]

    return "".join([col[0] for col in cols])


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
