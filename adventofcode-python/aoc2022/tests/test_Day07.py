from src.Day07 import part1, part2

input = """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
""".strip(
    "\n"
)


def test_one():
    assert part1(input) == 95437


def test_two():
    assert part2(input) == 24933642
