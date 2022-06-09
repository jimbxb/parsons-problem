#!/usr/bin/env python3 

import subprocess
import sys
import itertools

def main(argv):
    try:
        _, mode, solver, max_edits, soln_filepath, ans_filepath = argv

        if mode == "-i":
            process = ["swipl", "-s", solver, "--"]
        elif mode == "-c":
            process = [solver]
        else: 
            raise Exception("bad arguments")

        with open(soln_filepath, "r") as soln_fp, open(ans_filepath, "r") as ans_fp:
            soln, ans_combinations = preprocess(soln_fp.readlines(), ans_fp.readlines())

            max_score, max_msg = 0, "too far gone"
            for ans in ans_combinations:
                try:
                    output = subprocess.check_output(process + [max_edits, soln, ans])
                    output = output.decode("utf-8").split("\n")
                    score = int(output[0])
                    if score > max_score:
                        max_score = score
                        max_msg = output[1]
                except subprocess.CalledProcessError as e:
                    pass
            print(f"{max_score}\n{max_msg}")
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        print("Usage: [-i <solver script>|-c <solver exe>] <max edits> <solution> <answer>", file=sys.stderr)


def preprocess(soln, ans):
    soln_indents, soln_content = indent_content(soln)
    ans_indents, ans_content = indent_content(ans)

    soln_indexed = list(enumerate(soln_content))
    possible_indexes = []
    for idx, line in enumerate(ans_content):
        possible = [soln_idx for soln_idx, soln_line in soln_indexed 
                    if line == soln_line]
        possible_indexes.append(possible or ['x'])

    return (
        prolog_pairs(range(len(soln_content)), soln_indents),
        [prolog_pairs(ans_indexes, ans_indents) 
            for ans_indexes in itertools.product(*possible_indexes)
            if len(ans_indexes) == len(set(ans_indexes))]
    )


def indent_content(lines):
    indents = [indentation(line) for line in lines]
    non_zero = list(filter(lambda x: x != 0, indents))
    indent = min(non_zero) if non_zero else 1 
    
    return (
        [ind // indent for ind in indents], 
        [line.lstrip(" ").rstrip("\n") for line in lines]
    )


def indentation(line):
    return len(line) - len(line.lstrip(" "))


def prolog_pairs(*iters):
    return f"[{','.join('-'.join(map(str, pairs)) for pairs in zip(*iters))}]"


if __name__ == "__main__":
    main(sys.argv)