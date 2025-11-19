# code to convert the very bad .edge into .pel
# seems like
# conversion is done by swapping comma for space
# only unique entries taken i.e undirected graphs

edges = set()

with open("test.edges", "r") as fin:
    for line in fin:
        if not line.strip():
            continue
        u, v = map(int, line.strip().split(","))
        edges.add(tuple(sorted((u, v))))

with open("test.pel", "w") as fout:
    for u, v in sorted(edges):
        fout.write(f"{u} {v}\n")

print(f"Converted {len(edges)} unique edges to output.pel")
