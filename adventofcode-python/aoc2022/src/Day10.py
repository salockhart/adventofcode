from aocd import data, submit


def execute(input, on_cycle):
    cycle = 0
    signal = 1

    instructions = input.splitlines()
    addx = None

    while len(instructions) > 0:
        cycle += 1

        on_cycle(cycle, signal)

        if addx != None:
            signal += addx
            addx = None
        else:
            instruction = instructions.pop(0)
            match instruction.split(" "):
                case ["addx", x]:
                    addx = int(x)


def part1(input):
    signal_hist = []

    def on_cycle(cycle, signal):
        if cycle in [20, 60, 100, 140, 180, 220]:
            signal_hist.append(cycle * signal)

    execute(input, on_cycle=on_cycle)

    return sum(signal_hist)


def part2(input):
    crt = []

    def on_cycle(cycle, signal):
        horizontal_pos = (cycle - 1) % 40
        vertical_pos = (cycle - 1) // 40

        if len(crt) <= vertical_pos:
            crt.append([])

        crt[vertical_pos].insert(
            horizontal_pos,
            "#" if horizontal_pos in [signal - 1, signal, signal + 1] else ".",
        )

    execute(input, on_cycle=on_cycle)

    print("\n".join(["".join(row) for row in crt]))


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    # print("part 2: %s" % part2Answer)
