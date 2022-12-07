from src.Day04 import part1, part2

input = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
""".strip(
    "\n"
)


def test_one():
    assert part1(input) == 2


def test_two():
    assert part2(input) == 4
