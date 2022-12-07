from aocd import data, submit


def get_dir_sizes(input):
    pwd = []
    dir_sizes = {}
    for line in input.splitlines():
        match line.split():
            case ["$", "cd", ".."]:
                pwd.pop()
            case ["$", "cd", dir]:
                pwd.append(dir)
            case ["$", "ls"]:
                continue
            case ["dir", _]:
                continue
            case [size, _]:
                for path in ("/".join(pwd[0:i]) for i in range(len(pwd), 0, -1)):
                    if path not in dir_sizes:
                        dir_sizes[path] = 0
                    dir_sizes[path] += int(size)
    return dir_sizes


def part1(input):
    dir_sizes = get_dir_sizes(input)

    return sum([dir_sizes[path] for path in dir_sizes if dir_sizes[path] < 100000])


def part2(input):
    dir_sizes = get_dir_sizes(input)

    unused = 70000000 - dir_sizes["/"]
    target = 30000000 - unused

    return min([dir_sizes[path] for path in dir_sizes if dir_sizes[path] >= target])


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
