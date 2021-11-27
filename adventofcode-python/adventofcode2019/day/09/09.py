import fileinput
from collections import defaultdict

POSITION = 0
IMMEDIATE = 1
RELATIVE = 2

ADD = 1
MUL = 2
IN = 3
OUT = 4
JUMP_TRUE = 5
JUMP_FALSE = 6
LESS_THAN = 7
EQUALS = 8
ADD_RELATIVE_BASE = 9
HALT = 99

READ = 0
WRITE = 1

OPS = {
    ADD: (READ, READ, WRITE),
    MUL: (READ, READ, WRITE),
    IN: (WRITE,),
    OUT: (READ,),
    JUMP_TRUE: (READ, READ),
    JUMP_FALSE: (READ, READ),
    LESS_THAN: (READ, READ, WRITE),
    EQUALS: (READ, READ, WRITE),
    ADD_RELATIVE_BASE: (READ,),
    HALT: (),
}


def get_args(pc, base, codes, arg_kinds, modes):
    args = [None] * 4

    for i, kind in enumerate(arg_kinds):
        a = codes[pc + 1 + i]
        modes, mode = divmod(modes, 10)

        if mode == RELATIVE:
            a += base

        if mode in (POSITION, RELATIVE):
            if a < 0:
                raise Exception(
                    f"Invalid access to negative memory index: {a}")

            if kind == READ:
                a = codes[a]
            elif kind != WRITE:
                raise Exception(f"Invalid arg kind: {kind}")

        elif mode == IMMEDIATE:
            if kind == WRITE:
                raise Exception(f"Invalid arg mode for write arg: {mode}")
        else:
            raise Exception(f"Invalid arg mode: {mode}")

        args[i] = a

    return args


def execute(codes, inputs, initial_pc=0, initial_base=0):
    pc = initial_pc
    base = initial_base

    while codes[pc] != 99:
        modes, opcode = divmod(codes[pc], 100)

        if opcode not in OPS:
            raise Exception(f"Unknown opcode: {opcode}")

        arg_kinds = OPS[opcode]
        a, b, c, _ = get_args(pc, base, codes, arg_kinds, modes)
        pc += 1 + len(arg_kinds)

        if opcode == IN:
            codes[a] = inputs.pop()
        elif opcode == OUT:
            print(a)
            return pc, base, codes, a
        elif opcode == ADD:
            codes[c] = a + b
        elif opcode == MUL:
            codes[c] = a * b
        elif opcode == LESS_THAN:
            codes[c] = 1 if a < b else 0
        elif opcode == EQUALS:
            codes[c] = 1 if a == b else 0
        elif opcode == JUMP_TRUE:
            if a != 0:
                pc = b
        elif opcode == JUMP_FALSE:
            if a == 0:
                pc = b
        elif opcode == ADD_RELATIVE_BASE:
            base += a
        else:
            raise Exception(f"Unimplemented opcode: {opcode}")

    return pc, base, codes, None


def part1(lines):
    codes = defaultdict(int)
    for i, code in enumerate(",".join(line.strip() for line in lines).split(",")):
        codes[i] = int(code)

    last_output = -1
    idx = 0
    base = 0

    while last_output is not None:
        idx, base, codes, last_output = execute(codes, [1], idx, base)

    return ""


def part2(lines):
    codes = defaultdict(int)
    for i, code in enumerate(",".join(line.strip() for line in lines).split(",")):
        codes[i] = int(code)

    last_output = -1
    idx = 0
    base = 0

    while last_output is not None:
        idx, base, codes, last_output = execute(codes, [2], idx, base)

    return ""


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
