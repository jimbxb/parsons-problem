#!/usr/bin/env python3 

import itertools
import subprocess
import sys


PROLOG = ["swipl"]


def solve(solver, max_edits, ans_path, sol_paths):
    max_score, max_msg = 0, "too far gone"
    breaker = False
    with open(ans_path, "r") as ans_fp:
        raw_ans = ans_fp.readlines()
        for sol_path in sol_paths:
            with open(sol_path, "r") as sol_fp:
                sol, ans_combinations = preprocess(sol_fp.readlines(), raw_ans)
                for ans in ans_combinations:
                    try:
                        output = subprocess.check_output(solver + [max_edits, sol, ans])
                        output = output.decode("utf-8").split("\n")
                        
                        score = int(output[0])
                        if score > max_score:
                            max_score = score
                            max_msg = output[1]

                        if output[0] == max_edits:
                            breaker = True
                            break 
                    except subprocess.CalledProcessError as e:
                        pass
            if breaker: break
    print(f"{max_score}\n{max_msg}")


def preprocess(sol, ans):
    sol_indents, sol_content = indent_content(sol)
    ans_indents, ans_content = indent_content(ans)

    sol_indexed = list(enumerate(sol_content))
    possible_indexes = [[sol_idx for sol_idx, sol_line in sol_indexed 
                         if line == sol_line] or ['x']
                        for idx, line in enumerate(ans_content)]

    return (
        prolog_pairs(range(len(sol_content)), sol_indents),
        [prolog_pairs(ans_indexes, ans_indents) 
            for ans_indexes in itertools.product(*possible_indexes)
            if len(ans_indexes) == len(set(ans_indexes))]
    )


def indent_content(lines):
    indents = [len(line) - len(line.lstrip(" ")) for line in lines]
    non_zero = list(filter(lambda x: x != 0, indents))
    indent = min(non_zero) if non_zero else 1 
    
    return (
        [ind // indent for ind in indents], 
        [line.lstrip(" ").rstrip("\n") for line in lines]
    )


def prolog_pairs(*iters):
    return f"[{','.join('-'.join(map(str, pairs)) for pairs in zip(*iters))}]"


if __name__ == "__main__":
    try:
        _, mode, solver_path, max_edits, *sol_paths, ans_path = sys.argv

        if mode == "-i":
            solver = PROLOG + ["-t", "halt", "-s", solver_path, "--"]
        elif mode == "-c":
            solver = [solver_path]
        else: 
            raise Exception("bad arguments")

        solve(solver, max_edits, ans_path, sol_paths)
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        print("Usage: [-i <script>|-c <exe>] <max edits> <answer> <solutions>", 
              file=sys.stderr)
