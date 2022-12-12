from src.Day12 import part1, part2

input = """
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
""".strip(
    "\n"
)


def test_one():
    assert part1(input) == 31


def test_two():
    assert part2(input) == 29
