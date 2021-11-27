import fileinput
import operator


def val(codes, mode, value):
    if mode == 0:
        return codes[value]
    elif mode == 1:
        return value


def mode(modes, idx):
    if idx >= len(modes):
        return 0

    return modes[idx]


def calc(opcode, modes, i, op, codes):
    a = codes[i+1]
    b = codes[i+2]
    c = codes[i+3]
    lhs = val(codes, mode(modes, 0), a)
    rhs = val(codes, mode(modes, 1), b)
    out = op(lhs, rhs)

    codes[c] = out
    return (codes, i + 4)


def jmp(opcode, modes, i, op, codes):
    a = codes[i+1]
    b = codes[i+2]

    cond = val(codes, mode(modes, 0), a)
    dest = val(codes, mode(modes, 1), b)

    if op(cond, 0):
        return (codes, dest)
    else:
        return (codes, i + 3)


def compare(opcode, modes, i, op, codes):
    a = codes[i+1]
    b = codes[i+2]
    c = codes[i+3]

    lhs = val(codes, mode(modes, 0), a)
    rhs = val(codes, mode(modes, 1), b)
    out = 1 if op(lhs, rhs) else 0

    codes[c] = out

    return (codes, i + 4)


def execute(codes, input_value):
    i = 0
    while codes[i] != 99:
        opcode = codes[i] % 100
        modes = list(reversed([int(x) for x in str(codes[i] // 100)]))

        if opcode == 1:
            codes, i = calc(opcode, modes, i, operator.add, codes)
        elif opcode == 2:
            codes, i = calc(opcode, modes, i, operator.mul, codes)
        elif opcode == 3:
            a = codes[i+1]
            i += 2
            codes[a] = input_value
        elif opcode == 4:
            a = codes[i+1]
            i += 2
            print("output: %s" % val(codes, mode(modes, 0), a))
        elif opcode == 5:
            codes, i = jmp(opcode, modes, i, operator.ne, codes)
        elif opcode == 6:
            codes, i = jmp(opcode, modes, i, operator.eq, codes)
        elif opcode == 7:
            codes, i = compare(opcode, modes, i, operator.lt, codes)
        elif opcode == 8:
            codes, i = compare(opcode, modes, i, operator.eq, codes)

    return codes


def part1(lines):
    codes = [int(code) for code in ",".join(lines).split(",")]
    execute(codes, 1)[0]


def part2(lines):
    codes = [int(code) for code in ",".join(lines).split(",")]
    execute(codes, 5)[0]


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % part1(lines))
    print("part 2: %s" % (part2(lines),))
