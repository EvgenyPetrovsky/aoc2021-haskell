module Day07 (
  part1,
  part2,
  dayNo
) where

import qualified Lib as L
import qualified Data.List as DL
import Data.Text (unpack, pack, splitOn)

dayNo = 7 :: Int

type Answer = Int
type Solution = (Input -> Answer)

printPartN :: Solution -> IO ()
printPartN solution = do
  input <- L.loadFile dayNo
  let answer = solution . parseInput $ input
  putStr . show $ answer

part1 :: IO ()
part1 = printPartN solvePart_1

part2 :: IO ()
part2 = printPartN solvePart_2

{- Define data types -}

type Position = Int

newtype Input = Input [Position]

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  Input positions
  where
    del = pack ","
    positions = map (read . unpack) . splitOn del . pack $ head xs

solvePart_1 :: Solution
solvePart_1 (Input i) =
  sum . map (part1Cost bestPosition) $ i
  where 
    bestPosition = part1BestPosition i

solvePart_2 :: Solution
solvePart_2 (Input i) = 
  -- brute force solution of checking all positions
  minimum . map (\bestPosition -> sum . map (part2Cost bestPosition) $ i) $ position
  where 
    position = [0..(maximum i)]

{- Supplementary functions -}

part1BestPosition :: [Position] -> Position
part1BestPosition ps =
  sorted !! middle
  where
    sorted = DL.sort ps
    middle = length ps `div` 2
  
part1Cost :: Position -> Position -> Int
part1Cost p1 p2 = abs (p1 - p2)
  
part2BestPosition :: [Position] -> Position
part2BestPosition ps =
  d + a
  where 
    l = length ps
    (d,r) = (sum ps) `divMod` l
    a = if (r * 2) >= l then 1 else 0

part2Cost :: Position -> Position -> Int
part2Cost p1 p2 = 
  sum [1..diff]
  where 
  diff = abs (p1 - p2)
