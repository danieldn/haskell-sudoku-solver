# Haskell Sudoku Solver

## Overview

This solver implements the backtracking algorithm to solving a 9x9 sudoku puzzle.

Example psuedo code for backtracking from
https://codemyroad.wordpress.com/2014/05/01/solving-sudoku-by-backtracking/

```
function backtrack(position){
    if (isEndOfGrid == true){ // Empty cells filled. Solution found. Abort
        return true;
    }
 
    foreach (x from 1 ... 9){
        grid[position] = x;
        if (gridIsValid == true){ // Check for collisions
            if (backtrack(nextPosition) == true){ // Move to next empty cell
                return true; // Empty cells filled. Solution found. Abort.
            }
        }
    }
    grid[position] = NULL; // Empties cell
    return false; //Solution not found. Backtrack.
```

## Source Files

Solver.hs - Contains `main`, file IO functions, and support functions to build a sudoku puzzle

Puzzle.hs - Contains data types, backtracking algorithm, and support functions to model a 9x9 sudoku puzzle

## User Guide

Assumes GHCi, version 8.2.2

User may compile and run program by specifying a number of files containing each
containing one sudoku puzzle.

Example:
```
ghc -O solver.hs
./solver puzzle1.txt puzzle2.txt puzzle3.txt
```

Alternatively, user may load program from GHCi. 

```
ghci
:l Solver.hs
:set args puzzle1.txt puzzle2.txt puzzle3.txt
main
```

If no args are given, user will be asked to enter a puzzle.
```
*Main> main
Enter a sudoku puzzle row by row
Use whitespace to seperate values
Use 0 to indicate empty cell
----9x9 example----
5 3 0  0 7 0  0 0 0
6 0 0  1 9 5  0 0 0
0 9 8  0 0 0  0 6 0
8 0 0  0 6 0  0 0 3
4 0 0  8 0 3  0 0 1
7 0 0  0 2 0  0 0 6
0 6 0  0 0 0  2 8 0
0 0 0  4 1 9  0 0 5
0 0 0  0 8 0  0 7 9
-------------------
```
The program will print out a solved sudoku puzzle.
```
[5,3,4,6,7,8,9,1,2]
[6,7,2,1,9,5,3,4,8]
[1,9,8,3,4,2,5,6,7]
[8,5,9,7,6,1,4,2,3]
[4,2,6,8,5,3,7,9,1]
[7,1,3,9,2,4,8,5,6]
[9,6,1,5,3,7,2,8,4]
[2,8,7,4,1,9,6,3,5]
[3,4,5,2,8,6,1,7,9]
```
If an invalid board is entered, an error message will appear.
```
Entered...

[0,7,7,2,3,8,0,0,0]
[0,0,0,7,4,0,8,0,9]
[0,6,8,1,0,9,0,0,2]
[0,3,5,4,0,0,0,0,8]
[6,0,7,8,0,2,5,0,1]
[8,0,0,0,0,5,7,6,0]
[2,0,0,6,0,3,1,9,0]
[7,0,9,0,2,1,0,0,0]
[0,0,0,9,7,4,0,8,0]

Solving...

Board entered is invalid.
```