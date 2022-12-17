# import pyparsing as pp
from aocd import data, submit
from pyparsing import pyparsing_common

signed_int = pyparsing_common.signed_integer

beacon = (
    "Sensor at x="
    + signed_int
    + ", y="
    + signed_int
    + ": closest beacon is at x="
    + signed_int
    + ", y="
    + signed_int
)


def parse(input: str):
    return [
        (
            (sensor_x, sensor_y),
            (beacon_x, beacon_y),
            abs(sensor_y - beacon_y) + abs(sensor_x - beacon_x),
        )
        for _, sensor_x, _, sensor_y, _, beacon_x, _, beacon_y in [
            beacon.parse_string(line) for line in input.splitlines()
        ]
    ]


def part1(row: int, input: str):
    hits = set()

    beacons = []
    sensors = parse(input)

    for ((sensor_x, sensor_y), (beacon_x, beacon_y), distance) in sensors:
        if beacon_y == row:
            beacons.append(beacon_x)

        if row in range(sensor_y - distance, sensor_y + distance + 1):
            horizontal_distance = distance - abs(sensor_y - row)
            left_bound, right_bound = (
                sensor_x - horizontal_distance,
                sensor_x + horizontal_distance,
            )
            for i in range(left_bound, right_bound + 1):
                hits.add(i)

    return len([x for x in hits if x not in beacons])


def part2(max: int, input: str):
    sensors = parse(input)

    for ((sensor_x, sensor_y), _, distance) in sensors:
        outside_range = distance + 1
        outside_coords = set(
            (x, y)
            for x in range(sensor_x - outside_range, sensor_x + outside_range + 1)
            for y in [
                sensor_y + (outside_range - abs(sensor_x - x)),
                sensor_y - (outside_range - abs(sensor_x - x)),
            ]
            if x in range(0, max + 1) and y in range(0, max + 1)
        )
        for (option_x, option_y) in outside_coords:
            for ((sensor_x, sensor_y), _, distance_to_beacon) in sensors:
                distance_to_option = abs(sensor_y - option_y) + abs(sensor_x - option_x)
                if distance_to_option <= distance_to_beacon:
                    break
            else:
                return 4000000 * option_x + option_y


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(2000000, data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(4000000, data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
