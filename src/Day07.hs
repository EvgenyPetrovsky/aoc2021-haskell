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
    bestPosition = median i

solvePart_2 :: Solution
solvePart_2 (Input i) = 
  sum . map (part2Cost bestPosition) $ i
  where 
    bestPosition = average i

{- Supplementary functions -}

-- in our case doesn't really metter to take element on left or right 
-- when number is even
median :: [Position] -> Position
median ps =
  sorted !! middle
  where
    sorted = DL.sort ps
    middle = length ps `div` 2
  
part1Cost :: Position -> Position -> Int
part1Cost p1 p2 = abs (p1 - p2)

-- for some reason besto positions is not an arithmetically rounded average
-- but truncated average number :-(  
average :: [Position] -> Position
average ps =
  d + a
  where 
    l = length ps
    (d,r) = (sum ps) `divMod` l
    --a = if (r * 2) >= l then 1 else 0
    a = 0

part2Cost :: Position -> Position -> Int
part2Cost p1 p2 = 
  sum [1..diff]
  where 
  diff = abs (p1 - p2)
