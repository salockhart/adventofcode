from aocd import data, submit
from pyparsing import Literal, Word, alphas, delimited_list, pyparsing_common

number = pyparsing_common.number
valve = Word(alphas)

valveSchema = (
    "Valve"
    + valve
    + "has flow rate="
    + number
    + (Literal("; tunnels lead to valves") ^ Literal("; tunnel leads to valve"))
    + delimited_list(valve)[1, ...]
)


def parse(input: str):
    cave = {}
    for line in input.splitlines():
        _, valve, _, flow_rate, _, *to_valves = valveSchema.parse_string(line)
        cave[valve] = {"flow_rate": flow_rate, "to_valves": to_valves}
    return cave


def round(i: int, position: str, cave: dict):
    
    if i == 0:
        return 0


def part1(input: str):
    return round(30, "AA", parse(input))

    # cave = parse(input)
    # position = "AA"

    # total = 0
    # pressure = 0
    # open_valves = []

    # for minute in range(30):
    #     print("== Minute %s ==" % (minute + 1))

    #     match (len(open_valves)):
    #         case 0:
    #             print("No valves are open.")
    #         case _:
    #             print(
    #                 "Valve(s) %s open, releasing %s pressure."
    #                 % (", ".join(open_valves), pressure)
    #             )
    #             total += pressure

    #     this_flow_rate = cave[position]["flow_rate"]

    #     # nope, this greedy approach doesn't seem to work
    #     neighbours = sorted(
    #         # hmm
    #         filter(
    #             lambda x: x[1] > 2 * this_flow_rate,
    #             [(n, cave[n]["flow_rate"]) for n in cave[position]["to_valves"]],
    #         ),
    #         key=lambda x: x[1],
    #         reverse=True,
    #     )
    #     print(neighbours)

    #     if len(neighbours) > 0:
    #         (neighbour, _) = neighbours[0]
    #         print("You move to valve %s." % neighbour)
    #         position = neighbour
    #     else:
    #         print("You open valve %s." % position)
    #         open_valves = open_valves + [position]
    #         pressure += this_flow_rate
    #         cave[position]["flow_rate"] = 0

    #     print()

    # print(cave)

    # return None


def part2(input):
    return None


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
