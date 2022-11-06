import fileinput


def dijkstra(graph, source):
    visited = {source: 0}
    path = {}

    nodes = set(graph.keys())

    while nodes:
        min_node = None
        for node in nodes:
            if node in visited:
                if min_node is None:
                    min_node = node
                elif visited[node] < visited[min_node]:
                    min_node = node

        if min_node is None:
            break

        nodes.remove(min_node)
        current_weight = visited[min_node]

        for child in graph[min_node]:
            weight = current_weight + 1
            if child not in visited or weight < visited[child]:
                visited[child] = weight
                path[child] = min_node

    return visited, path


def graph(lines):
    orbits = {}

    for line in lines:
        [inside, outside] = line.strip().split(")")

        if inside not in orbits:
            orbits[inside] = []

        if outside not in orbits:
            orbits[outside] = []

        orbits[inside].append(outside)
        orbits[outside].append(inside)

    return orbits


def part1(lines):
    return sum(dijkstra(graph(lines), "COM")[0].values())


def part2(lines):
    return dijkstra(graph(lines), "YOU")[0]["SAN"] - 2


parts = (part1, part2)


if __name__ == "__main__":
    lines = list(fileinput.input())
    print("part 1: %s" % (part1(lines),))
    print("part 2: %s" % (part2(lines),))
