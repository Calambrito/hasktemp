def read_cliques(filename):
    cliques = []
    with open(filename, 'r') as f:
        for line in f:
            if "nodes" not in line:
                continue
            nums = line.split()[1:]  # skip the word "nodes"
            if not nums:
                continue
            cliques.append([int(n) for n in nums])
    return cliques


def read_edges(filename):
    edges = set()
    with open(filename, 'r') as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) == 2:
                a, b = int(parts[0]), int(parts[1])
                edges.add((a, b))
                edges.add((b, a))  # undirected
    return edges


def density(clique, edges):
    n = len(clique)
    if n < 2:
        return 0.0
    actual_edges = sum(
        1 for i in range(n) for j in range(i + 1, n)
        if (clique[i], clique[j]) in edges
    )
    return actual_edges / (n * (n - 1) / 2)


def main():
    cliques = read_cliques("output.txt")
    edges = read_edges("test.pel")

    for clique in cliques:
        d = density(clique, edges)
        print(f"nodes {', '.join(map(str, clique))} -> density = {d:.3f}")


if __name__ == "__main__":
    main()
