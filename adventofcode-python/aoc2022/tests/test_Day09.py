from src.Day09 import part1, part2

input1 = """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
""".strip(
    "\n"
)

input2 = """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
""".strip(
    "\n"
)


def test_one():
    assert part1(input1) == 13


def test_two():
    assert part2(input1) == 1
    assert part2(input2) == 36
