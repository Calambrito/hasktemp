#!/usr/bin/env python3
import re

FLOAT_RE = re.compile(r'^[+-]?(?:\d+\.\d*|\d*\.\d+)(?:[eE][+-]?\d+)?$')

def is_float_token(tok: str) -> bool:
    """Return True if tok looks like a float/double (has a decimal point or exponent)."""
    return bool(FLOAT_RE.match(tok))

def process_line(line: str):
    """Return tuple (first_node_id_or_None, processed_line_str)."""
    tokens = line.strip().split()
    if not tokens:
        return (None, "")  # empty line

    # find index of first float-like token
    idx = None
    for i, tok in enumerate(tokens):
        if is_float_token(tok):
            idx = i
            break

    if idx is None:
        # no float found: treat whole line as integer tokens
        int_tokens = tokens
        rest_tokens = []
    else:
        int_tokens = tokens[:idx]
        rest_tokens = tokens[idx:]

    # try convert int_tokens to ints; if any token is non-int, keep original order
    ints = []
    all_ints = True
    for t in int_tokens:
        try:
            ints.append(int(t))
        except ValueError:
            all_ints = False
            break

    if all_ints:
        ints.sort()  # ascending
        sorted_int_tokens = [str(x) for x in ints]
    else:
        # fallback: keep them as original tokens (no sort) to avoid data corruption
        sorted_int_tokens = int_tokens

    processed_tokens = sorted_int_tokens + rest_tokens
    first_node_id = None
    if sorted_int_tokens:
        try:
            first_node_id = int(sorted_int_tokens[0])
        except ValueError:
            first_node_id = None

    processed_line = " ".join(processed_tokens)
    return (first_node_id, processed_line)

def main():
    infile = "final.txt"
    outfile = "ordered.txt"

    with open(infile, "r", encoding="utf-8") as f:
        lines = f.readlines()

    if not lines:
        print("Input file is empty.")
        return

    header = lines[0].rstrip("\n")
    body_lines = lines[1:]

    processed = []
    for raw in body_lines:
        # skip pure-empty lines but preserve them as empty entries
        if raw.strip() == "":
            processed.append((None, ""))
            continue
        first_id, new_line = process_line(raw)
        processed.append((first_id, new_line))

    # sort lines by first_node_id descending; lines with None go to the end, preserve relative order among None
    processed_with_key = [(pid if pid is not None else -10**30, i, pid, ln)
                          for i, (pid, ln) in enumerate(processed)]
    # We used index i to stabilize sort for None entries
    processed_with_key.sort(key=lambda x: (x[0]), reverse=True)

    sorted_lines = [entry[3] for entry in processed_with_key]

    with open(outfile, "w", encoding="utf-8") as f:
        f.write(header + "\n")
        for ln in sorted_lines:
            f.write(ln.rstrip() + "\n")

    print(f"Wrote sorted output to '{outfile}'")

if __name__ == "__main__":
    main()
