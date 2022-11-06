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


def parse(lines):
    codes = defaultdict(int)
    for i, code in enumerate(",".join(line.strip() for line in lines).split(",")):
        codes[i] = int(code)
    return codes


def display(chart, position):
    blocks = ["█", "-", "*"]

    x_values = [x for x, _ in chart.keys()]
    y_values = [y for _, y in chart.keys()]

    min_x = min(x_values)
    max_x = max(x_values)

    min_y = min(y_values)
    max_y = max(y_values)

    print(chr(27) + "[2J")
    output = ""
    for y in reversed(range(min_y, max_y + 1)):
        for x in range(min_x, max_x + 1):
            if (x, y) == (0, 0):
                output += "•"
            elif (x, y) == position:
                output += "D"
            elif (x, y) not in chart:
                output += " "
            else:
                output += blocks[chart[(x, y)]]
        output += "\n"
    print(output, flush=True)
    time.sleep(0.01)


COUNTERPARTS = {1: 2, 2: 1, 3: 4, 4: 3}


def move(direction, position):
    if direction == 1:
        return (position[0], position[1] + 1)
    elif direction == 2:
        return (position[0], position[1] - 1)
    elif direction == 3:
        return (position[0] + 1, position[1])
    elif direction == 4:
        return (position[0] - 1, position[1])


def dijkstra(chart, source):
    visited = {source: 0}
    path = {}

    nodes = set(chart.keys())

    while nodes:
        min_node = None
        for node in nodes:
            if node in visited:
                if min_node is None:
                    min_node = node
                elif visited[node] < visited[min_node]:
                    min_node = node

        if min_node is None:
            break

        nodes.remove(min_node)
        current_weight = visited[min_node]

        adjacents = [move(x + 1, min_node) for x in range(4)]
        for child in adjacents:
            if child not in chart:
                continue
            elif chart[child] == 0:
                continue

            weight = current_weight + 1
            if child not in visited or weight < visited[child]:
                visited[child] = weight
                path[child] = min_node

    return visited, path


def repair(lines):
    codes = parse(lines)
    idx = 0
    base = 0

    chart = {}
    position = (0, 0)
    oxygen = (0, 0)
    direction = 1

    history = []

    chart[position] = 1

    backtracking = False

    while True:
        idx, base, codes, output = execute(codes, [direction], idx, base)

        # display(chart, position)

        new_position = move(direction, position)
        chart[new_position] = output

        if output != 0:
            if not backtracking:
                history.append(direction)
            position = new_position

        if output == 2:
            oxygen = position

        adjacents = [move(x + 1, position) for x in range(4)]
        possibles = [i + 1 for i, p in enumerate(adjacents) if p not in chart]

        if len(possibles) == 0:
            backtracking = True
            if len(history) == 0:
                break

            direction = COUNTERPARTS[history.pop()]
        else:
            backtracking = False
            direction = possibles.pop()

    return chart, oxygen


def part1(lines):
    chart, oxygen = repair(lines)
    return dijkstra(chart, (0, 0))[0][oxygen]


def part2(lines):
    chart, oxygen = repair(lines)
    return max(dijkstra(chart, oxygen)[0].values())


parts = (part1, part2)


if __name__ == "__main__":
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
