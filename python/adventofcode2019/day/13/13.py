import fileinput
import time
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


def parse(lines):
    codes = defaultdict(int)
    for i, code in enumerate(",".join(line.strip() for line in lines).split(",")):
        codes[i] = int(code)
    return codes


def display(screen, score):
    blocks = [' ', '█', '▄', '▔', '•']

    x_values = [x for x, _ in screen.keys()]
    y_values = [y for _, y in screen.keys()]

    min_x = min(x_values)
    max_x = max(x_values)

    min_y = min(y_values)
    max_y = max(y_values)

    print(chr(27) + "[2J")
    print(f'score: {score}')
    output = ''
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            output += blocks[screen[(x, y)]]
        output += '\n'
    print(output, flush=True)
    time.sleep(0.01)


def play(lines, quarters):
    codes = parse(lines)
    codes[0] = quarters

    screen = defaultdict(int)
    score = 0

    idx = 0
    base = 0
    booted = False

    joystick = 0
    paddle_pos = (0, 0)
    ball_pos = (0, 0)

    while True:
        idx, base, codes, x = execute(codes, [joystick], idx, base)

        if x is None:
            break

        idx, base, codes, y = execute(codes, [], idx, base)
        idx, base, codes, tileId = execute(codes, [], idx, base)

        if (x, y) == (-1, 0):
            score = tileId
            continue

        if tileId == 3:
            paddle_pos = (x, y)
        elif tileId == 4:
            ball_pos = (x, y)

        screen[(x, y)] = tileId

        if paddle_pos[0] < ball_pos[0]:
            joystick = 1
        elif paddle_pos[0] > ball_pos[0]:
            joystick = -1
        else:
            joystick = 0

        if booted and 3 in screen.values() and 4 in screen.values():
            display(screen, score)

        if (x, y) == (36, 21):
            booted = True

    return screen, score


def part1(lines):
    screen, _ = play(lines, 1)

    return len(list(filter(lambda key: screen[key] == 2, screen.keys())))


def part2(lines):
    _, score = play(lines, 2)

    return score


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
