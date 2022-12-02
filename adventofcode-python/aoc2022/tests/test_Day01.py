from src.Day01 import part1, part2

input = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
""".strip()


def test_one():
    assert part1(input) == 24000


def test_two():
    assert part2(input) == 45000
