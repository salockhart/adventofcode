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
                raise Exception(f"Invalid access to negative memory index: {a}")

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
            # print(a)
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


DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]


def paint(lines, initial_panel):
    codes = defaultdict(int)
    for i, code in enumerate(",".join(line.strip() for line in lines).split(",")):
        codes[i] = int(code)

    position = (0, 0)
    direction = DIRECTIONS[0]
    hull = defaultdict(int, {position: initial_panel})

    idx = 0
    base = 0
    while True:
        current_panel = hull[position]

        idx, base, codes, out = execute(codes, [current_panel], idx, base)

        if out is None:
            break

        hull[position] = out

        idx, base, codes, out = execute(codes, [current_panel], idx, base)

        dir_idx = DIRECTIONS.index(direction)
        direction = DIRECTIONS[(dir_idx + [-1, 1][out]) % len(DIRECTIONS)]

        position = (position[0] + direction[0], position[1] + direction[1])

    return hull


def part1(lines):
    hull = paint(lines, 0)

    return len(hull.keys())


def part2(lines):
    hull = paint(lines, 1)

    x_values = [x for x, _ in hull.keys()]
    y_values = [y for _, y in hull.keys()]

    min_x = min(x_values)
    max_x = max(x_values)

    min_y = min(y_values)
    max_y = max(y_values)

    for y in reversed(range(min_y, max_y + 1)):
        for x in range(min_x, max_x + 1):

            print([" ", "█"][hull[(x, y)]], end="")
        print()

    return ""


parts = (part1, part2)


if __name__ == "__main__":
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
