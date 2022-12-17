from src.Day14 import part1, part2

input = """
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
""".strip(
    "\n"
)


def test_one():
    assert part1(input) == 24


def test_two():
    assert part2(input) == 93
