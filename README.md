
# Parsons Problem Solver

A simple Parsons problem solver written in Prolog, with a helper script in Python.

## Usage
```sh
> swipl -O -o parsons -c parsons.pl
> ./parsons.py -c ./parsons 10 sol*.txt ans.txt
8
[swap(5,8),indent(10,1)]
```