import fileinput
import itertools
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


def execute(codes, inputs, initial=0):
    i = initial
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
            codes[a] = inputs.pop(0)
        elif opcode == 4:
            a = codes[i+1]
            i += 2
            out = val(codes, mode(modes, 0), a)
            return i, codes, out
        elif opcode == 5:
            codes, i = jmp(opcode, modes, i, operator.ne, codes)
        elif opcode == 6:
            codes, i = jmp(opcode, modes, i, operator.eq, codes)
        elif opcode == 7:
            codes, i = compare(opcode, modes, i, operator.lt, codes)
        elif opcode == 8:
            codes, i = compare(opcode, modes, i, operator.eq, codes)

    return i, codes, None


def part1(lines):
    codes = [int(code) for code in ",".join(lines).split(",")]

    max_output = 0

    for phases in list(itertools.permutations([0, 1, 2, 3, 4])):

        last_output = 0

        for phase in phases:
            _, _, last_output = execute(codes, [phase, last_output])

        if last_output > max_output:
            max_output = last_output

    return max_output


def part2(lines):
    codes = [int(code) for code in ",".join(lines).split(",")]

    max_output = 0

    for phases in list(itertools.permutations([5, 6, 7, 8, 9])):
        saved_state = {}
        for i in range(5):
            saved_state[i] = (0, codes.copy())

        halted = []

        last_output = 0
        inputs = [[x] for x in phases]

        while len(halted) < len(phases):
            for idx, _ in enumerate(phases):
                if idx in halted:
                    continue

                if last_output is not None:
                    inputs[idx] += [last_output]

                i, state, last_output = execute(saved_state[idx][1],
                                                inputs[idx],
                                                saved_state[idx][0])

                saved_state[idx] = (i, state)

                if last_output is None:
                    halted.append(idx)

            if last_output is not None and last_output > max_output:
                max_output = last_output

    return max_output


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
