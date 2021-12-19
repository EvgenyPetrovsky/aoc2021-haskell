module Day01 (
  part1,
  part2
) where

import qualified Lib as L

dayN = 1 :: Int
type Input = Int
type Answer = Int
type Solution = ([Input] -> Answer)
parseInput :: [String] -> [Input]
parseInput = map read

printPartN :: Solution -> IO ()
printPartN solution = do
  input <- L.loadFile dayN
  let answer = solution . parseInput $ input
  putStr . show $ answer

part1 :: IO ()
part1 = printPartN solvePart_1

part2 :: IO ()
part2 = printPartN solvePart_2

{-
Take depths
and compare each value with its previous value
and count only those which are greater than previous value
-}
solvePart_1 :: Solution
solvePart_1 ds =
  length . filter (\(a, b) -> b > a) $ zip ds ds1
  where
    ds1 = drop 1 ds

{-
Take 3 consequtive depths and sum them up do it for all items (sliding window)
Use acquired items as imput for solution 1
-}
solvePart_2 :: [Input] -> Answer
solvePart_2 ds =
  solvePart_1 dsAvg
    where
      dsAvg = map (\(a, b, c) -> a + b + c) dsSum3
      dsSum3 = zip3 ds (drop 1 ds) (drop 2 ds)
