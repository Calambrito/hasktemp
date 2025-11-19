import numpy as np
from itertools import combinations
import time
from collections import defaultdict
import re

def read_outputs_file_fast(filename):
    """Fast reading of output.txt using list comprehensions"""
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    pseudocliques = []
    for line in lines:
        parts = line.strip().split()
        if len(parts) < 2:
            continue
        nodes = set(map(int, parts[:-1]))
        density = float(parts[-1])
        pseudocliques.append((nodes, density))
    
    return pseudocliques

def filter_maximal_fast(pseudocliques):
    """Optimized maximal filtering"""
    # Sort by size descending for efficient checking
    pseudocliques.sort(key=lambda x: len(x[0]), reverse=True)
    
    maximal = []
    n = len(pseudocliques)
    
    for i in range(n):
        current_set, density = pseudocliques[i]
        is_maximal_flag = True
        
        # Only check against larger or equal sized sets that come before
        for j in range(i):
            other_set, _ = pseudocliques[j]
            if current_set.issubset(other_set):
                is_maximal_flag = False
                break
        
        if is_maximal_flag:
            # Check if any subsequent set of same size is a superset
            for j in range(i + 1, n):
                other_set, _ = pseudocliques[j]
                if len(other_set) < len(current_set):
                    break
                if current_set.issubset(other_set):
                    is_maximal_flag = False
                    break
            
            if is_maximal_flag:
                maximal.append((current_set, density))
    
    return maximal

def read_graph_file(filename):
    """Read the graph from test.pel file"""
    graph = defaultdict(set)
    nodes = set()
    
    with open(filename, 'r') as f:
        for line in f:
            # Extract all integers from the line
            numbers = list(map(int, re.findall(r'\d+', line)))
            if len(numbers) >= 2:
                u, v = numbers[0], numbers[1]
                graph[u].add(v)
                graph[v].add(u)
                nodes.add(u)
                nodes.add(v)
    
    return graph, nodes

def build_adjacency_matrix(graph, node_mapping):
    """Build adjacency matrix for faster density computation"""
    n = len(node_mapping)
    adj_matrix = np.zeros((n, n), dtype=bool)
    
    for node, neighbors in graph.items():
        if node in node_mapping:
            i = node_mapping[node]
            for neighbor in neighbors:
                if neighbor in node_mapping:
                    j = node_mapping[neighbor]
                    adj_matrix[i, j] = True
                    adj_matrix[j, i] = True
    
    return adj_matrix

def compute_density_fast(node_set, graph, node_mapping=None, adj_matrix=None):
    """Fast density computation using different strategies"""
    if len(node_set) <= 1:
        return 1.0 if len(node_set) == 1 else 0.0
    
    if adj_matrix is not None and node_mapping is not None:
        # Use adjacency matrix for speed
        indices = [node_mapping[node] for node in node_set if node in node_mapping]
        if len(indices) != len(node_set):
            # Some nodes not in mapping, fall back to graph method
            edge_count = 0
            node_list = list(node_set)
            for i, u in enumerate(node_list):
                for v in node_list[i+1:]:
                    if v in graph[u]:
                        edge_count += 1
        else:
            submatrix = adj_matrix[np.ix_(indices, indices)]
            edge_count = np.sum(submatrix) // 2
    else:
        # Use graph adjacency lists
        edge_count = 0
        node_list = list(node_set)
        for i, u in enumerate(node_list):
            for v in node_list[i+1:]:
                if v in graph[u]:
                    edge_count += 1
    
    max_possible_edges = len(node_set) * (len(node_set) - 1) / 2
    return edge_count / max_possible_edges if max_possible_edges > 0 else 0.0

def main_optimized():
    start_time = time.time()
    
    print("Reading and processing...")
    
    # Read data from output.txt (not outputs.txt)
    pseudocliques = read_outputs_file_fast('output.txt')
    print(f"Found {len(pseudocliques)} pseudocliques in output.txt")
    
    # Filter maximal cliques
    maximal_cliques = filter_maximal_fast(pseudocliques)
    print(f"Found {len(maximal_cliques)} maximal pseudocliques after filtering")
    
    # Read graph
    graph, all_nodes = read_graph_file('test.pel')
    print(f"Graph has {len(all_nodes)} nodes and {sum(len(adj) for adj in graph.values()) // 2} edges")
    
    # Create node mapping for matrix approach
    all_graph_nodes = sorted(all_nodes)
    node_mapping = {node: idx for idx, node in enumerate(all_graph_nodes)}
    
    # Build adjacency matrix (optional - use for large graphs)
    use_matrix = len(all_nodes) < 10000  # Only for reasonably sized graphs
    adj_matrix = None
    if use_matrix:
        print("Building adjacency matrix for faster computation...")
        adj_matrix = build_adjacency_matrix(graph, node_mapping)
    
    # Verify densities
    mismatches = 0
    with open('cliques.txt', 'w') as f:  # Changed to cliques.txt
        f.write("Maximal Pseudocliques Analysis\n")
        f.write("=" * 100 + "\n")
        f.write(f"{'Nodes':<60} {'Original Density':<18} {'Computed Density':<18} {'Match':<8}\n")
        f.write("-" * 100 + "\n")
        
        for node_set, original_density in maximal_cliques:
            if use_matrix:
                computed_density = compute_density_fast(node_set, graph, node_mapping, adj_matrix)
            else:
                computed_density = compute_density_fast(node_set, graph)
            
            match = abs(original_density - computed_density) < 1e-4
            
            if not match:
                mismatches += 1
                print(f"ERROR: Density mismatch for nodes {sorted(node_set)}")
                print(f"  Original: {original_density:.6f}, Computed: {computed_density:.6f}")
            
            # Format node list for output - print ALL nodes without truncation
            sorted_nodes = sorted(node_set)
            nodes_str = ' '.join(map(str, sorted_nodes))
            
            f.write(f"{nodes_str:<60} {original_density:<18.6f} {computed_density:<18.6f} {str(match):<8}\n")
    
    end_time = time.time()
    print(f"\nOptimized processing completed in {end_time - start_time:.2f} seconds")
    print(f"Found {len(maximal_cliques)} maximal pseudocliques")
    print(f"Detected {mismatches} density mismatches")
    print(f"Results written to cliques.txt")

# Alternative simpler version if you prefer
def main_simple():
    """Simpler version without advanced optimizations"""
    start_time = time.time()
    
    # Read outputs from output.txt
    pseudocliques = []
    with open('output.txt', 'r') as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) < 2:
                continue
            nodes = set(map(int, parts[:-1]))
            density = float(parts[-1])
            pseudocliques.append((nodes, density))
    
    print(f"Found {len(pseudocliques)} pseudocliques")
    
    # Filter maximal (simpler approach)
    pseudocliques.sort(key=lambda x: len(x[0]), reverse=True)
    maximal_cliques = []
    
    for i, (current_set, density) in enumerate(pseudocliques):
        is_maximal = True
        for j, (other_set, _) in enumerate(pseudocliques):
            if i != j and current_set.issubset(other_set):
                is_maximal = False
                break
        if is_maximal:
            maximal_cliques.append((current_set, density))
    
    print(f"Found {len(maximal_cliques)} maximal pseudocliques")
    
    # Read graph
    graph = defaultdict(set)
    nodes = set()
    with open('test.pel', 'r') as f:
        for line in f:
            numbers = list(map(int, re.findall(r'\d+', line)))
            if len(numbers) >= 2:
                u, v = numbers[0], numbers[1]
                graph[u].add(v)
                graph[v].add(u)
                nodes.add(u)
                nodes.add(v)
    
    print(f"Graph has {len(nodes)} nodes")
    
    # Verify densities
    mismatches = 0
    with open('cliques.txt', 'w') as f:  # Changed to cliques.txt
        f.write("Maximal Pseudocliques Analysis (Simple)\n")
        f.write("=" * 100 + "\n")
        f.write(f"{'Nodes':<60} {'Original Density':<18} {'Computed Density':<18} {'Match':<8}\n")
        f.write("-" * 100 + "\n")
        
        for node_set, original_density in maximal_cliques:
            # Compute density
            node_list = list(node_set)
            edge_count = 0
            for i, u in enumerate(node_list):
                for v in node_list[i+1:]:
                    if v in graph[u]:
                        edge_count += 1
            
            max_edges = len(node_set) * (len(node_set) - 1) / 2
            computed_density = edge_count / max_edges if max_edges > 0 else 0.0
            
            match = abs(original_density - computed_density) < 1e-4
            
            if not match:
                mismatches += 1
                print(f"ERROR: Density mismatch for {len(node_set)} nodes")
                print(f"  Original: {original_density:.6f}, Computed: {computed_density:.6f}")
            
            # Print ALL nodes without truncation
            sorted_nodes = sorted(node_set)
            nodes_str = ' '.join(map(str, sorted_nodes))
            
            f.write(f"{nodes_str:<60} {original_density:<18.6f} {computed_density:<18.6f} {str(match):<8}\n")
    
    end_time = time.time()
    print(f"\nSimple processing completed in {end_time - start_time:.2f} seconds")
    print(f"Detected {mismatches} density mismatches")

if __name__ == "__main__":
    # Use the optimized version by default
    main_optimized()
    
    # Uncomment below to use the simpler version instead
    # main_simple()