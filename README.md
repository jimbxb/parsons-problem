
# Parsons Problem Solver

A simple Parsons problem solver written in Prolog, with a helper script in Python.

## Usage
```sh
> swipl -O -o parsons -c parsons.pl
> ./parsons.py -c ./parsons 10 soln.txt ans.txt
6 
[indent(12,3),cycle(7,13),cycle(11,12),cycle(9,7)]
```