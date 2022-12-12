import sys

from aocd import data, submit


def sum_coords(a, b):
    return (a[0] + b[0], a[1] + b[1])


def parse(input):
    height_map = {
        (x, y): h
        for y, line in enumerate(input.splitlines())
        for x, h in enumerate(line)
    }
    start_pos = None
    end_pos = None
    for coord in height_map:
        if height_map[coord] == "S":
            start_pos = coord
            height_map[coord] = 1
            continue
        elif height_map[coord] == "E":
            end_pos = coord
            height_map[coord] = 26
            continue
        height_map[coord] = ord(height_map[coord]) - 96
    return height_map, start_pos, end_pos


def dijkstra(graph, start_node, get_neighbors, distance):
    unvisited_nodes = graph
    shortest_path = {}
    previous_nodes = {}
    for coord in unvisited_nodes:
        shortest_path[coord] = sys.maxsize
    shortest_path[start_node] = 0

    while unvisited_nodes:
        current_min_node = None
        for coord in unvisited_nodes:
            if current_min_node == None:
                current_min_node = coord
            elif shortest_path[coord] < shortest_path[current_min_node]:
                current_min_node = coord

        neighbors = get_neighbors(graph, current_min_node)
        for neighbor in neighbors:
            tentative_value = shortest_path[current_min_node] + distance(
                current_min_node, neighbor
            )
            if tentative_value < shortest_path[neighbor]:
                shortest_path[neighbor] = tentative_value
                previous_nodes[neighbor] = current_min_node

        unvisited_nodes.pop(current_min_node)

    return previous_nodes, shortest_path


def part1(input):
    height_map, start_pos, end_pos = parse(input)

    _, shortest_path = dijkstra(
        height_map,
        start_node=start_pos,
        get_neighbors=lambda graph, node: [
            n
            for n in [sum_coords(node, n) for n in [(0, 1), (0, -1), (1, 0), (-1, 0)]]
            if n in graph and (graph[node] - graph[n]) >= -1
        ],
        distance=lambda a, b: 1,
    )

    return shortest_path[end_pos]


def part2(input):
    height_map, _, end_pos = parse(input)

    target_coords = [x for x in height_map if height_map[x] == 1]

    _, shortest_path = dijkstra(
        height_map,
        start_node=end_pos,
        get_neighbors=lambda graph, node: [
            n
            for n in [sum_coords(node, n) for n in [(0, 1), (0, -1), (1, 0), (-1, 0)]]
            if n in graph and (graph[node] - graph[n]) <= 1
        ],
        distance=lambda a, b: 1,
    )

    return min(
        [shortest_path[coord] for coord in shortest_path if coord in target_coords]
    )


parts = (part1, part2)


if __name__ == "__main__":
    part1Answer = part1(data)
    submit(part1Answer, part="a")
    print("part 1: %s" % part1Answer)

    part2Answer = part2(data)
    submit(part2Answer, part="b")
    print("part 2: %s" % part2Answer)
