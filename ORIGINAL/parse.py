def parse_node_sets(filename):
    unique_sets = []
    with open(filename, 'r') as f:
        for line in f:
            idx = line.find("nodes")
            if idx == -1:
                continue
            node_part = line[idx + len("nodes"):].strip()
            numbers = node_part.split()
            if len(numbers) != 10:
                continue
            sorted_nums = tuple(sorted(int(n) for n in numbers))
            if sorted_nums not in unique_sets:
                unique_sets.append(sorted_nums)
    return unique_sets


def read_edges(filename):
    edges = []
    with open(filename, 'r') as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) != 2:
                continue
            a, b = int(parts[0]), int(parts[1])
            edges.append((a, b))
    return edges


def compute_density(node_set, edges):
    n = len(node_set)
    if n < 2:
        return 0.0
    node_set = set(node_set)
    actual_edges = 0
    for a, b in edges:
        if a in node_set and b in node_set:
            actual_edges += 1
    max_edges = n * (n - 1) / 2
    return actual_edges / max_edges


def main():
    node_sets_file = "output.txt"
    edges_file = "test.pel"

    node_sets = parse_node_sets(node_sets_file)
    edges = read_edges(edges_file)

    print("Found", len(node_sets), "unique node sets\n")
    for i, node_set in enumerate(node_sets, 1):
        density = compute_density(node_set, edges)
        print("Set", i, "Density:", round(density, 3))


if __name__ == "__main__":
    main()
