from aocd import data, submit


def parse(input):
    cave = {}

    cave[(500, 0)] = "+"

    for line in input.splitlines():
        segments = [
            list(map(int, segment.split(","))) for segment in line.split(" -> ")
        ]
        for i in range(0, len(segments) - 1):
            [from_x, from_y] = segments[i]
            [to_x, to_y] = segments[i + 1]
            [from_x, to_x] = sorted([from_x, to_x])
            [from_y, to_y] = sorted([from_y, to_y])
            for tile in [
                (x, y) for x in range(from_x, to_x + 1) for y in range(from_y, to_y + 1)
            ]:
                cave[tile] = "#"
    return cave


def cave_bounds(cave):
    from_x, to_x, from_y, to_y = (500, 500, 0, 0)
    for (x, y) in cave:
        from_x = min(from_x, x)
        to_x = max(to_x, x)
        from_y = min(from_y, y)
        to_y = max(to_y, y)
    return from_x, to_x, from_y, to_y


def neighbours(coord):
    left = (coord[0] - 1, coord[1])
    right = (coord[0] + 1, coord[1])
    return left, right


def part1(input):
    cave = parse(input)

    i = 0
    sand_pos = (500, 0)
    while True:
        (sand_pos_x, sand_pos_y) = sand_pos
        tiles_below = [
            (tile_x, tile_y)
            for (tile_x, tile_y) in cave
            if tile_x == sand_pos_x and tile_y > sand_pos_y
        ]

        if len(tiles_below) == 0:
            return i

        tile_below = min(tiles_below, key=lambda tile: tile[1])
        tile_left = (tile_below[0] - 1, tile_below[1])
        tile_right = (tile_below[0] + 1, tile_below[1])

        if tile_left not in cave:
            sand_pos = tile_left
        elif tile_right not in cave:
            sand_pos = tile_right
        else:
            cave[(tile_below[0], tile_below[1] - 1)] = "o"
            i += 1
            sand_pos = (500, 0)


def part2(input):
    cave = parse(input)
    _, _, _, max_y = cave_bounds(cave)

    def set_tile_if_not_present(coord, value):
        if coord not in cave:
            cave[coord] = value

    cave[(500, 0)] = "o"
    for y in range(0, max_y + 2):
        set_tile_if_not_present((500, y), "o")
        for j in range(0, y + 1):
            set_tile_if_not_present((500 + j, y), "o")
            set_tile_if_not_present((500 - j, y), "o")

    for y in range(1, max_y + 2):
        row = [coord for coord in cave if coord[1] == y and cave[coord] == "o"]
        for tile in row:
            above = (tile[0], tile[1] - 1)
            above_left, above_right = neighbours(above)
            num_feeders = len(
                [
                    x
                    for x in [above_left, above, above_right]
                    if x in cave and cave[x] == "o"
                ]
            )
            if num_feeders == 0:
                cave.pop(tile)

    return len([x for x in cave if cave[x] == "o"])


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
