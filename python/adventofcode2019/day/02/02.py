import fileinput


def execute(codes):
    i = 0
    while codes[i] != 99:
        opcode = codes[i]
        a = codes[i+1]
        b = codes[i+2]
        c = codes[i+3]

        if opcode == 1:
            codes[c] = codes[a] + codes[b]
        elif opcode == 2:
            codes[c] = codes[a] * codes[b]

        i += 4

    return codes


def part1(lines):
    codes = [int(code) for code in ",".join(lines).split(",")]
    codes[1] = 12
    codes[2] = 2
    return execute(codes)[0]


def part2(lines):
    for noun in range(0, 100):
        for verb in range(0, 100):
            codes = [int(code) for code in ",".join(lines).split(",")]
            codes[1] = noun
            codes[2] = verb
            res = execute(codes)[0]
            if res == 19690720:
                return (noun, verb, 100 * noun + verb)


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % part1(lines))
    print("part 2: %s" % (part2(lines),))
