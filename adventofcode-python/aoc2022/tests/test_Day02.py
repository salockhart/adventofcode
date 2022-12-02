from src.Day02 import part1, part2

input = """
A Y
B X
C Z
""".strip()


def test_one():
    assert part1(input) == 15


def test_two():
    assert part2(input) == 12
