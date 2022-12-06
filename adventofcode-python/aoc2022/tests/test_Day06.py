from src.Day06 import part1, part2

input1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
input2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
input3 = "nppdvjthqldpwncqszvftbrmjlhg"
input4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
input5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"


def test_one():
    assert part1(input1) == 7
    assert part1(input2) == 5
    assert part1(input3) == 6
    assert part1(input4) == 10
    assert part1(input5) == 11


def test_two():
    assert part2(input1) == 19
    assert part2(input2) == 23
    assert part2(input3) == 23
    assert part2(input4) == 29
    assert part2(input5) == 26
