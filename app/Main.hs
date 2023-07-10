module Main (main) where

import Lib
import Sudoku

main :: IO ()
main = do
    putStrLn (unlines $ head $ solve4 hard)
