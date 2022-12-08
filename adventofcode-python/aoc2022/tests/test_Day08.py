from src.Day08 import part1, part2

input = """
30373
25512
65332
33549
35390
""".strip(
    "\n"
)


def test_one():
    assert part1(input) == 21


def test_two():
    assert part2(input) == 8
