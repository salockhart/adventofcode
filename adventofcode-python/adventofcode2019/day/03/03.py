import fileinput

DX = {'L': -1, 'R': 1, 'U': 0, 'D': 0}
DY = {'L': 0, 'R': 0, 'U': 1, 'D': -1}


def draw(wire):
    ans = {}
    x = 0
    y = 0
    length = 0

    for vector in wire:
        direction = vector[0]
        distance = int(vector[1:])
        for _ in range(distance):
            x += DX[direction]
            y += DY[direction]
            length += 1
            if (x, y) not in ans:
                ans[(x, y)] = length

    return ans


def part1(lines):
    [wire1, wire2] = [line.strip().split(",") for line in lines]

    pointsA = draw(wire1)
    pointsB = draw(wire2)

    intersections = set(pointsA.keys()) & set(pointsB.keys())

    return min(abs(x) + abs(y) for (x, y) in intersections)


def part2(lines):
    [wire1, wire2] = [line.strip().split(",") for line in lines]

    pointsA = draw(wire1)
    pointsB = draw(wire2)

    intersections = set(pointsA.keys()) & set(pointsB.keys())

    return min(pointsA[(x, y)] + pointsB[(x, y)] for (x, y) in intersections)


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
