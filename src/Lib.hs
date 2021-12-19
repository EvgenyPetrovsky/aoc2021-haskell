module Lib
(
  loadFile
, Solvable
) where

type DayNumber = Int

loadFile :: DayNumber -> IO [String]

loadFile n =
    fmap lines (readFile file)
    where
        day_file = reverse . take 2 . reverse $ "0" ++ show n
        file = "./input/day_" ++ day_file

class Solvable i where
  parseInput :: [String] -> i
  solvePart1 :: i -> Int
  solvePart2 :: i -> Int
