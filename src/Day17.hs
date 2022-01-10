module Day17 (
  dayNo,
  part1,
  part2
) where

import qualified Lib
--import qualified Data.List as L
--import qualified Data.Map as M

dayNo = 17

type Answer = Int
type Solution = (Input -> Answer)

printPartN :: Solution -> IO ()
printPartN solution = do
  input <- Lib.loadFile dayNo
  let answer = solution . parseInput $ input
  putStr . show $ answer

part1 :: IO ()
part1 = printPartN solvePart_1

part2 :: IO ()
part2 = printPartN solvePart_2

{- Define data types -}

newtype Input = Input Area
data Area = Area { x :: (Int, Int), y :: (Int, Int) }

{- Solutions -}

parseInput :: [String] -> Input
parseInput _ =
  Input (Area {x = (79,137), y = (-176,-117)})

{-
  it is important that
    a) probe ends up in valid x range
    b) probe ends up at lowest possible y value from valid y range
  both conditions are not affecting each other
  so we ignore the physics of x coordinate and focus on y

-}
solvePart_1 :: Solution
solvePart_1 (Input i) =
  height speed_at_0_level
  where
    min_y = negate $ uncurry min (y i)
    speed_at_0_level = min_y - 1
    height :: Int -> Int
    height speed = if speed < 1 then 0 else speed + height (speed - 1)

{-
  Find all initial x speeds that lead to visit of target area,
  those are between 0 and max (x)
  Find all initial y speeds that lead to visit of target area,
  those are between - abs (min y) and abs (min y)
  make a product of them and count them
  ...
  not that easy - need to simulate the trajectory and know step and position
-}
solvePart_2 :: Solution
solvePart_2 (Input i) =
  error "not implemented"

{- Supplementary functions -}
