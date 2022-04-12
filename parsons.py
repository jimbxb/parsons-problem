#!/usr/bin/env python3 

import subprocess
import sys

def main(argv):
  try:
    _, mode, solver, max_edits, soln_filepath, ans_filepath = argv

    with open(soln_filepath, "r") as soln_fp, \
         open(ans_filepath, "r") as ans_fp:
      soln, ans = preprocess(soln_fp.readlines(), ans_fp.readlines())

      if mode == "-i":
        output = subprocess.check_output([
          "swipl", "-s", solver, 
          "--", str(max_edits), soln, ans
        ])
      elif mode == "-c":
        output = subprocess.check_output([solver, max_edits, soln, ans])
      else: 
        raise Exception("bad arguments")

      print(output.decode("utf-8"))
  except Exception as e:
    print(f"Error: {e}", file=sys.stderr)
    print("Usage: [-i <solver script>|-c <solver exe>] <max edits> <solution> <answer>", file=sys.stderr)


def preprocess(soln, ans):
  soln_indents, soln_content = indent_content(soln)
  ans_indents, ans_content = indent_content(ans)

  return (
    prolog_pairs(range(len(soln)), soln_indents), 
    prolog_pairs([soln_content.index(line) if line in soln_content else 'x'
                  for line in ans_content], ans_indents)
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