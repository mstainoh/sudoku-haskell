{-# LANGUAGE BlockArguments #-}
module Sudoku where


{-" 
Sudoku solver:

Main defs
    a grid is a set of rows, but can also be defined as a set of columns or set of boxes,
        Note: it is useful to think of boxes and columns as a function f: grid -> grid,
        such that the boxes become the rows, and such that f.f = id 
    a grid is valid if every box, row and column has no duplicates
    a grid G is a solution to a problem P if G is valid and if it matches the values of P (where available)
    a problem is solvable if it has at least one solution

Aux defs
    a grid A is a subgrid of B if all elements of B are elements of A (=> a solution of A is also a solution of B)
    a grid is void if one cell has no choices
    a grid is inconsistent if one row (or box or column) contains two cells with a single identical choice
    (=> a void or inconsistent grid in unsolvable)


Reasoning:
    we start by doing a very not intelligent solver that finds a solution in infinite time, we then reduce the search space
note: remember haskell is very smart in doing "all" and "any" in very big arrays
    
"-}

import Data.List (transpose, unfoldr, unlines)
import Data.List.Split (chunksOf)

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Value
type Value = Char
type Choices = [Value]

------ aux functions -------
composePred :: (Bool -> Bool -> Bool) -> [a -> Bool] -> [a] -> [Bool]
composePred b  fs xs = foldl1 (zipWith b) (map (`map` xs) fs)

single :: [a] -> Bool
single = (==1) . length

------ general functions -----------
printGrid :: Grid -> IO()
printGrid [] = do putStr ""
printGrid (x:xs) = do
    putStrLn x
    printGrid xs

gridLength :: Grid -> Int
gridLength [] = 0
gridLength (r:_) = length r

gridShapeCheck :: Grid -> Bool
gridShapeCheck [] = False
gridShapeCheck g =
    let n = gridLength g in
        let p x = length x == n in
            null $ dropWhile p g

---------------------------- split functions ----------------
rows :: Matrix a -> Matrix a
rows = id

columns :: Matrix a -> Matrix a
columns = transpose

-- splits a list into sublists of length n
--chunksOf :: Int -> [a] -> [[a]]
--chunksOf n x = takeWhile (not.null) $ unfoldr (Just . splitAt n) x

boxesN :: Int -> Matrix a -> Matrix a
-- from right to left:
-- -> create sublists of n (=3) elements in each row ([a] -> [[a]]: Row1 -> [s1, s2, s3])
-- -> transpose each sublist of sublists (this effectively creates a box)
-- -> concatenate back to reduce dimensionality
-- -> split in groups of n*n
boxesN n m = chunksOf (n*n) $ concatMap concat $ transpose $ map (chunksOf n) m

boxes :: Matrix a -> Matrix a
boxes m =
    map concat $ concatMap transpose $ chunksOf 3 $ map (chunksOf 3) m

---------- validation functions --------------
valid :: Grid -> Bool --grid is valid if there are no duplicates
valid g =
    all noDups (rows g) && all noDups (rows g) && all noDups (boxes g)

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (x:xs) = notElem x xs  && noDups xs

------------- solving functions ---------------
solveDumb :: Grid -> [Grid]
solveDumb g = filter valid $ collapse $ choices g

choices :: Grid -> Matrix Choices
choices =
    map (map choice)
    where
        choice :: Value -> Choices
        choice v = if v == '.' then ['1'..'9'] else [v]

collapse :: Matrix [a] -> [Matrix a] --matrix of choices -> choices of matrixes (insert meme here)
collapse m = cp (map cp m)
    where
        -- cp: cartesian product - recursive definition =)
        cp :: [[a]] -> [[a]]
        cp [] = [[]]
        cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

--------------- a smarter way to do it -------------------------
solve2 :: Grid -> [Grid]
solve2 g = filter valid $ collapse $ prune $ choices g

solve3 :: Grid -> [Grid]
solve3 g = filter valid $ collapse $ fix prune $ choices g

-- here is the MAGIC, we apply the function box, columns and rows twice, since f . f = id
prune :: Matrix Choices -> Matrix Choices
prune r = pruneBy rows $ pruneBy columns $ pruneBy boxes r
    where pruneBy f r = f $ map reduce (f r)

reduce :: Row Choices -> Row Choices
reduce r =
    let uniques = concat $ filter single r
        reducefunc x = if single x then x else filter (`notElem` uniques) x
    in
        map reducefunc r

-- fixed point
fix :: Eq a => (a -> a) -> a -> a
fix f x =
    if x == y then x else fix f y
    where y = f x

--------------- a yet smarter way to do it -------------------------
solve4 :: Grid -> [Grid]
solve4 g = search $  prune $ choices g

blocked :: Matrix Choices -> Bool
blocked m = (not.safe) m || void m
    where
        safe :: Matrix Choices -> Bool
        safe m = all consistent (rows m) && all consistent (boxes m) && all consistent (columns m)
            where
                consistent :: Row Choices -> Bool
                consistent m = noDups $ filter single m

        void :: Matrix Choices -> Bool
        void = any (any null)

search :: Matrix Choices -> [Grid]
search m
    | blocked m = []
    | all (all single) m = collapse m
    | otherwise =
        [g | m' <- expand m, g <- search (prune m')]

    where
        expand :: Matrix Choices -> [Matrix Choices]
        expand [] = [[]]
        expand (x:xs)
            |all single x = map (x:) (expand xs)
            |otherwise = map (:xs) (expandRow x)

expandRow :: Row Choices -> [Row Choices]
expandRow [] = [[]]
expandRow (x:xs)
    |single x = map (x:) (expandRow xs)
    |otherwise = [[head x] : xs, tail x : xs] :: [Row Choices]

-------------------------------
easy :: Grid
easy = [
    "53..7....",
    "6..195...",
    ".98....6.",
    "8...6...3",
    "4..8.3..1",
    "7...2...6",
    ".6....28.",
    "...419..5",
    "....8..79"
    ]

hard :: Grid
hard = [
    "...9.....",
    "2..4....6",
    "..5.7.3.8",
    ".7......4",
    "..3...1..",
    "..16...8.",
    ".....5.1.",
    "1.......9",
    "48...3.7."]

unsolvable = [
    "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
    ]

empty = replicate 9 $ concatMap (replicate 9) "."
    