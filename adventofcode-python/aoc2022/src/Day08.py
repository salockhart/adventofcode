from aocd import data, submit


def parse(input):
    rows = [
        [((x, y), int(c)) for (x, c) in enumerate(row)]
        for (y, row) in enumerate(input.splitlines())
    ]

    columns = [[row[i] for row in rows] for i in range(len(rows[0]))]

    return (rows, columns)


def mirror(grid):
    return [[r for r in reversed(row)] for row in grid]


def flatten(xss):
    return [x for xs in xss for x in xs]


def part1(input):
    rows, columns = parse(input)

    sightlines = rows + mirror(rows) + columns + mirror(columns)

    visible = set()

    for sightline in sightlines:
        max = -1
        for (coord, height) in sightline:
            if height > max:
                max = height
                visible.add(coord)

    return len(visible)


def part2(input):
    rows, _ = parse(input)

    trees = flatten(rows)

    candidates = [
        x
        for x in sorted(
            [
                t
                for t in trees
                if t[0][0] != 0
                and t[0][0] != (len(rows[0]) - 1)
                and t[0][1] != 0
                and t[0][1] != (len(rows) - 1)
            ],
            key=lambda x: x[1],
            reverse=True,
        )
    ]

    scores = []

    for (coord, height) in candidates:
        n_candidate = max(
            [
                x
                for x in trees
                if x[0][0] == coord[0] and x[0][1] < coord[1] and x[1] >= height
            ],
            key=lambda x: x[0][1],
            default=((coord[0], 0), 0),
        )
        s_candidate = min(
            [
                x
                for x in trees
                if x[0][0] == coord[0] and x[0][1] > coord[1] and x[1] >= height
            ],
            key=lambda x: x[0][1],
            default=((coord[0], len(rows) - 1), 0),
        )
        e_candidate = min(
            [
                x
                for x in trees
                if x[0][1] == coord[1] and x[0][0] > coord[0] and x[1] >= height
            ],
            key=lambda x: x[0][0],
            default=((len(rows[0]) - 1, coord[1]), 0),
        )
        w_candidate = max(
            [
                x
                for x in trees
                if x[0][1] == coord[1] and x[0][0] < coord[0] and x[1] >= height
            ],
            key=lambda x: x[0][0],
            default=((0, coord[1]), 0),
        )

        n_distance = coord[1] - n_candidate[0][1]
        s_distance = s_candidate[0][1] - coord[1]
        e_distance = e_candidate[0][0] - coord[0]
        w_distance = coord[0] - w_candidate[0][0]

        scores.append(n_distance * s_distance * e_distance * w_distance)

    return max(scores)


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
