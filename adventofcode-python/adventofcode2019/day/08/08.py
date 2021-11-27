import fileinput
import itertools
import operator


def part1(lines):
    width = 25
    height = 6
    layer_size = width * height

    digits = [int(x) for x in lines[0].strip()]
    layers = []

    for i in range(len(digits) // layer_size):
        start = i * layer_size
        end = (i + 1) * layer_size
        layers.append(digits[start:end])

    num_zeroes = [layer.count(0) for layer in layers]

    min_layer = layers[min(range(len(num_zeroes)), key=num_zeroes.__getitem__)]

    return min_layer.count(1) * min_layer.count(2)


def part2(lines):
    width = 25
    height = 6
    layer_size = width * height

    digits = [int(x) for x in lines[0].strip()]
    layers = []

    image = []

    for i in range(len(digits) // layer_size):
        start = i * layer_size
        end = (i + 1) * layer_size
        layers.append(digits[start:end])

    for i in range(layer_size):
        pixels = [layer[i] for layer in layers]
        image.append(next((x for x in pixels if x != 2), None))

    image = [[" ", "█"][x] for x in image]

    for i in range(layer_size):
        print(image[i], end=("\n" if ((i + 1) % width) == 0 else ""))

    return ""


parts = (part1, part2)


if __name__ == '__main__':
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
