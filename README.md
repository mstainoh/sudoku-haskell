# sudoku-haskell
implementation of a haskell solver based on Graham Hutton notes

Solver strategy:
1. implement box,column and row functions such that f.f = id
2. implement a naive solver that checks every possible grid combination and filters out invalid ones
3. implement a 2nd solver as the previous, adding a pruning function that eliminates grids that are already invalid based on the initial sudoku values
4. 3rd solver: same as before + fixed point pruning
5. implement a 4th solver that expands choices as follows:
    - if the grid is unsolvable (blocked) - no expansion (empty list)
    - if there is only one possible solution - no expansion (singleton list)
    - if there are more solutions: (recursively) branch the first possible choice and prune, continue until either a single solution or no solution is found 