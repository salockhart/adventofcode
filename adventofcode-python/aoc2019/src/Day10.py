import fileinput
import math
from collections import defaultdict


def parse(lines):
    asteroid_coords = []
    for y in range(len(lines)):
        line = lines[y].strip()
        for x in range(len(line)):
            if line[x] == "#":
                asteroid_coords.append((x, y))

    return asteroid_coords


def slope(a, b):
    if b[0] - a[0] == 0:
        return None
    else:
        return (b[1] - a[1]) / (b[0] - a[0])


def side(a, b):
    if a[1] == b[1]:
        return 1 if a[0] < b[0] else -1

    return 1 if a[1] < b[1] else -1


def to_angle(m, s):
    if m is None:
        angle = 90
    else:
        angle = math.atan(m) * 180 / math.pi

    if s == -1:
        angle -= 180

    return (angle + 90) % 360


def distance(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def part1(lines):
    asteroid_coords = parse(lines)

    detections = {}
    for coord in asteroid_coords:
        slopes = defaultdict(list)
        for p in asteroid_coords:
            if p in [coord]:
                continue

            m = slope(coord, p)
            s = side(coord, p)
            slopes[(m, s)].append(p)

        detections[coord] = len(slopes)

    best_location = max(detections.keys(), key=(lambda key: detections[key]))

    return best_location, detections[best_location]


def part2(lines):
    asteroid_coords = parse(lines)
    station = (25, 31)

    slopes = defaultdict(list)
    for p in asteroid_coords:
        if p in [station]:
            continue

        m = slope(station, p)
        s = side(station, p)
        slopes[(m, s)].append(p)

    slopes = {(to_angle(m, s)): slopes[(m, s)] for m, s in slopes}
    directions = sorted(slopes.keys())

    last_target = None
    num_destroyed = 0
    idx = 0
    while num_destroyed < 200:
        laser_dir = directions[idx % len(directions)]
        targets = slopes[laser_dir]

        last_target = target = min(
            targets, key=lambda target: distance(station, target)
        )

        targets.remove(target)

        if len(targets) == 0:
            directions.remove(laser_dir)
        else:
            idx += 1

        num_destroyed += 1

    return last_target, last_target[0] * 100 + last_target[1]


parts = (part1, part2)


if __name__ == "__main__":
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
