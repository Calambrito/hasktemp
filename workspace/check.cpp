// check.cpp
// Compile: g++ -O3 -std=c++17 -march=native -pipe -o check check.cpp
// Run:     ./check
//
// Assumptions & behavior:
// - Reads sets/densities from "output.txt". Each line: <node> <node> ... <density>
//   where node tokens are integers and last token is a floating point density.
// - Reads graph from "test.pel". Any integers found on a line: first two integers
//   are treated as an undirected edge (u v). (This follows the regex-extract approach.)
// - Node ids are normalized to integers and mapped to compact indices [0..N-1].
// - Maximal means maximal by set-inclusion.
// - Density computed as: (number_of_edges_in_induced_subgraph) / C(k,2).
//   For k <= 1 we use density = 0.0.
// - Two densities considered equal if they match to 4 decimal places
//   (i.e. round(x*1e4) == round(y*1e4)).
// - Outputs written to "final.txt" with columns:
//     node-list  reported_density  computed_density  EQUAL/DIFF
//
// Performance notes:
// - Uses vector<uint64_t> as compact bitmask for sets and adjacency bitsets.
// - Subset checking uses fast bitwise ops; popcount uses builtin intrinsics.

#include <bits/stdc++.h>
using namespace std;

using u64 = unsigned long long;

static inline int popcount_u64(u64 x) { return __builtin_popcountll(x); }

// --- Utility to split a vector<uint64_t> mask into a string key for dedupe map ---
string mask_key(const vector<u64>& mask) {
    string s;
    s.reserve(mask.size() * 20);
    for (u64 v : mask) {
        s += to_string(v);
        s.push_back(',');
    }
    return s;
}

// --- Read output.txt: parse lines into vector of (vector<int> nodes, double density, original_line) ---
struct RawSet {
    vector<long long> nodes;
    double density;
    string original_line;
};

vector<RawSet> read_output_file(const string& filename) {
    vector<RawSet> res;
    ifstream ifs(filename);
    if (!ifs) {
        cerr << "Error: cannot open " << filename << "\n";
        return res;
    }
    string line;
    while (getline(ifs, line)) {
        string trimmed = line;
        // trim whitespace at ends
        while (!trimmed.empty() && isspace((unsigned char)trimmed.back())) trimmed.pop_back();
        size_t p = 0;
        while (p < trimmed.size() && isspace((unsigned char)trimmed[p])) ++p;
        if (p) trimmed = trimmed.substr(p);
        if (trimmed.empty()) continue;
        // split on whitespace
        stringstream ss(trimmed);
        vector<string> toks;
        string tk;
        while (ss >> tk) toks.push_back(tk);
        if (toks.size() < 2) continue;
        // last token is density
        try {
            double d = stod(toks.back());
            vector<long long> nodes;
            nodes.reserve(toks.size()-1);
            bool ok = true;
            for (size_t i = 0; i+1 < toks.size(); ++i) {
                // convert to long long (assuming nodes are integers)
                try {
                    long long v = stoll(toks[i]);
                    nodes.push_back(v);
                } catch (...) { ok = false; break; }
            }
            if (!ok) continue;
            sort(nodes.begin(), nodes.end());
            nodes.erase(unique(nodes.begin(), nodes.end()), nodes.end());
            res.push_back({nodes, d, trimmed});
        } catch (...) {
            // skip lines that do not end with density
            continue;
        }
    }
    return res;
}

// --- Read graph file test.pel. We'll extract all integers from each line and take first two as edge. ---
vector<pair<long long,long long>> read_graph_edges(const string& filename, unordered_set<long long>& node_pool) {
    vector<pair<long long,long long>> edges;
    ifstream ifs(filename);
    if (!ifs) {
        cerr << "Warning: cannot open " << filename << " (graph will be empty)\n";
        return edges;
    }
    string line;
    std::regex re(R"(([-]?\d+))");
    while (getline(ifs, line)) {
        vector<long long> nums;
        for (sregex_iterator it(line.begin(), line.end(), re), end; it != end; ++it) {
            nums.push_back(stoll((*it)[1].str()));
        }
        if (nums.size() >= 2) {
            long long u = nums[0], v = nums[1];
            edges.emplace_back(u, v);
            node_pool.insert(u);
            node_pool.insert(v);
        }
    }
    return edges;
}

// --- Convert sets to bitmask representation given node->index mapping ---
vector<u64> make_mask_from_nodes(const vector<long long>& nodes, const unordered_map<long long,int>& idx_map, int words) {
    vector<u64> mask(words, 0ULL);
    for (auto id : nodes) {
        auto it = idx_map.find(id);
        if (it == idx_map.end()) continue; // node not present in mapping (shouldn't happen)
        int idx = it->second;
        int w = idx >> 6;
        int b = idx & 63;
        mask[w] |= (1ULL << b);
    }
    return mask;
}

int popcount_mask(const vector<u64>& mask) {
    int s = 0;
    for (u64 v : mask) s += popcount_u64(v);
    return s;
}

// check if mask_a is subset of mask_b (i.e., a <= b)
bool mask_is_subset(const vector<u64>& a, const vector<u64>& b) {
    int W = (int)a.size();
    for (int i = 0; i < W; ++i) {
        if ((a[i] & ~b[i]) != 0ULL) return false;
    }
    return true;
}

// bitwise AND of two masks -> result stored into out (out = a & b)
void mask_and(const vector<u64>& a, const vector<u64>& b, vector<u64>& out) {
    int W = (int)a.size();
    for (int i = 0; i < W; ++i) out[i] = a[i] & b[i];
}

// Helper to count edges in induced subgraph using adjacency bitsets
// adj_bit[u] is a mask of neighbors of u (size W)
// maskS is the set mask
// We'll compute sum_{u in S} popcount(adj_bit[u] & maskS) and then divide by 2.
long long induced_edge_count(const vector<vector<u64>>& adj_bit, const vector<int>& set_indices, const vector<u64>& maskS) {
    long long sum = 0;
    int W = (int)maskS.size();
    vector<u64> tmp(W);
    for (int idx : set_indices) {
        // tmp = adj_bit[idx] & maskS
        mask_and(adj_bit[idx], maskS, tmp);
        // count bits
        for (u64 v : tmp) sum += popcount_u64(v);
    }
    // each edge counted twice
    return sum / 2;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    const string outputs_fname = "output.txt";
    const string graph_fname = "test.pel";
    const string out_fname = "final.txt";

    // 1) Read output.txt
    auto raw_sets = read_output_file(outputs_fname);
    cerr << "Parsed " << raw_sets.size() << " lines from " << outputs_fname << "\n";

    // 2) Read graph edges and build node pool
    unordered_set<long long> node_pool;
    for (const auto &rs : raw_sets) {
        for (auto n : rs.nodes) node_pool.insert(n);
    }
    auto edges = read_graph_edges(graph_fname, node_pool);
    cerr << "Graph parsing: collected " << node_pool.size() << " unique nodes from output+graph, and " << edges.size() << " edges (raw)\n";

    // 3) Build node -> index mapping
    vector<long long> node_list;
    node_list.reserve(node_pool.size());
    for (auto v : node_pool) node_list.push_back(v);
    sort(node_list.begin(), node_list.end()); // deterministic ordering
    unordered_map<long long,int> idx;
    idx.reserve(node_list.size()*2);
    for (int i = 0; i < (int)node_list.size(); ++i) idx[node_list[i]] = i;
    int N = (int)node_list.size();
    if (N == 0) {
        cerr << "Warning: no nodes found. Exiting.\n";
        return 0;
    }
    int W = (N + 63) / 64;

    // 4) Convert raw sets into masks; deduplicate by mask string key, keep first density
    struct SetRec { vector<u64> mask; int pop; vector<long long> original_nodes; double density; string original_line; };
    vector<SetRec> recs;
    recs.reserve(raw_sets.size());
    unordered_set<string> seen_keys;
    seen_keys.reserve(raw_sets.size()*2);

    for (const auto& rs : raw_sets) {
        auto mask = make_mask_from_nodes(rs.nodes, idx, W);
        string key = mask_key(mask);
        if (seen_keys.find(key) != seen_keys.end()) continue; // dedupe
        seen_keys.insert(key);
        // count popcount
        int p = popcount_mask(mask);
        recs.push_back({move(mask), p, rs.nodes, rs.density, rs.original_line});
    }
    cerr << "After deduplication: " << recs.size() << " unique sets\n";

    // 5) Sort by popcount descending (largest first). If equal size, deterministic tie-break by lexicographic node list.
    sort(recs.begin(), recs.end(), [](const SetRec& a, const SetRec& b){
        if (a.pop != b.pop) return a.pop > b.pop;
        // tie-break: compare node list string
        // both original_nodes are sorted earlier in read, so we can lexicographically compare
        return a.original_nodes < b.original_nodes;
    });

    // 6) Maximal filtering: keep a set only if it's not a subset of any previously kept set.
    vector<SetRec> maximal;
    maximal.reserve(recs.size());
    for (size_t i = 0; i < recs.size(); ++i) {
        const auto &cur = recs[i];
        bool is_subset = false;
        for (const auto &k : maximal) {
            if (mask_is_subset(cur.mask, k.mask)) { is_subset = true; break; }
        }
        if (!is_subset) maximal.push_back(cur);
    }
    cerr << "Kept " << maximal.size() << " maximal sets after filtering\n";

    // 7) Build adjacency bitsets from edges
    vector<vector<u64>> adj_bit(N, vector<u64>(W, 0ULL));
    for (auto &e : edges) {
        auto itu = idx.find(e.first);
        auto itv = idx.find(e.second);
        if (itu == idx.end() || itv == idx.end()) continue;
        int u = itu->second;
        int v = itv->second;
        int wu = v >> 6; int bu = v & 63;
        int wv = u >> 6; int bv = u & 63;
        adj_bit[u][wu] |= (1ULL << bu);
        adj_bit[v][wv] |= (1ULL << bv);
    }
    // Note: isolated nodes will have zero adjacency.

    // 8) For each maximal set compute density and compare
    ofstream ofs(out_fname);
    if (!ofs) { cerr << "Error opening " << out_fname << " for write\n"; return 1; }
    ofs << "# node-list  reported_density  computed_density  EQUAL/DIFF\n";

    int mismatches = 0;
    for (const auto &rec : maximal) {
        // get indices of nodes present in this set
        vector<int> indices;
        indices.reserve(rec.pop);
        // iterate through mask to extract indices efficiently
        for (int w = 0; w < W; ++w) {
            u64 word = rec.mask[w];
            while (word) {
                int bit = __builtin_ctzll(word);
                int idx_global = (w << 6) + bit;
                if (idx_global < N) indices.push_back(idx_global);
                word &= word - 1;
            }
        }
        int k = (int)indices.size();
        long long edgecount = 0;
        if (k <= 1) {
            edgecount = 0;
        } else {
            edgecount = induced_edge_count(adj_bit, indices, rec.mask);
        }
        double denom = (k <= 1) ? 1.0 : ( (double)k * (k - 1) / 2.0 );
        double computed_density = (k <= 1) ? 0.0 : ( (double)edgecount / denom );

        // equality to 4 decimal places
        long long r1 = llround(rec.density * 1e4);
        long long r2 = llround(computed_density * 1e4);
        bool equal4 = (r1 == r2);

        if (!equal4) {
            ++mismatches;
            cout << "ERROR: Density mismatch for nodes (size " << k << "):\n  reported=" << fixed << setprecision(6) << rec.density
                 << " computed=" << computed_density << "\n";
            // print a short node list for debugging
            // reconstruct printable nodes from indices by comparing mask
            // We'll print using original_nodes stored
            cout << "  set sample: ";
            int cnt = 0;
            for (auto v : rec.original_nodes) {
                if (cnt++ >= 20) { cout << "..."; break; }
                cout << v << " ";
            }
            cout << "\n";
        }

        // prepare node list string (sorted numeric)
        vector<long long> nodes_out = rec.original_nodes;
        sort(nodes_out.begin(), nodes_out.end());
        string nodes_s;
        for (size_t i = 0; i < nodes_out.size(); ++i) {
            if (i) nodes_s.push_back(' ');
            nodes_s += to_string(nodes_out[i]);
        }
        // write to final.txt
        ofs << nodes_s << " "
            << fixed << setprecision(6) << rec.density << " "
            << fixed << setprecision(6) << computed_density << " "
            << (equal4 ? "EQUAL" : "DIFF") << "\n";
    }

    cerr << "Done. Checked " << maximal.size() << " maximal sets; mismatches=" << mismatches
         << ". Results in " << out_fname << "\n";
    return 0;
}
