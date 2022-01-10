module Day11 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
--import qualified Data.List as L
import qualified Data.Map as M

dayNo = 11

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

newtype Input = Input OctopusGrid

type Coordinate = (Int, Int)
type OctopusGrid = M.Map Coordinate EnergyLevel
data EnergyLevel =
  Lev Int | Flash | Tired
  deriving (Eq, Show)

gridSize :: Int
gridSize = 10 :: Int

{- Solutions -}

parseInput :: [String] -> Input
parseInput ls = Input . M.fromList $ allCoordinates `zip` ns
  where
    -- flat list of numbers
    ns = map (\x -> Lev (read [x])) (concat ls)

solvePart_1 :: Solution
solvePart_1 (Input grid) =
  total_flashes
  where
    (total_flashes, _) = simulateNSteps 100 grid

simulateNSteps :: Int -> OctopusGrid -> (Int, OctopusGrid)
simulateNSteps n grid =
  iter n grid 0
  where
    iter :: Int -> OctopusGrid -> Int -> (Int, OctopusGrid)
    iter n grid acc =
      if n <= 0 then (acc, grid)
      else iter (n - 1) new_grid new_acc
      where
        new_grid = singleStep grid
        new_acc = acc + countFlashTrails new_grid

solvePart_2 :: Solution
solvePart_2 (Input grid) = simulateUntilSync grid

simulateUntilSync :: OctopusGrid -> Int
simulateUntilSync = iter 0
  where
    iter :: Int -> OctopusGrid -> Int
    iter n grid =
      if countFlashTrails grid == 100 then n
      else iter (n + 1) (singleStep grid)

{- Supplementary functions -}

allCoordinates :: [Coordinate]
allCoordinates = [(x, y) | x <- take gridSize [0..],
                           y <- take gridSize [0..]]

adjacentCoordinates :: Coordinate -> [Coordinate]
adjacentCoordinates (x, y) =
  [(x1, y1) | x1 <- [x-1..x+1], y1 <- [y-1..y+1],
              x /= x1 || y /= y1,
              x >= 0 && x < gridSize, y >= 0 && y < gridSize ]

singleStep :: OctopusGrid -> OctopusGrid
singleStep = resetGridEnergyLevel . propagateFlashEffect . increaseGridEnergyLevel

increaseEnergyLevel :: EnergyLevel -> EnergyLevel
increaseEnergyLevel x =
  case x of
    Lev 9 -> Flash
    Lev n -> Lev (n+1)
    _ -> x

deactivateFlash :: EnergyLevel -> EnergyLevel
deactivateFlash Flash = Tired
deactivateFlash other = other

resetEnergyLevel :: EnergyLevel -> EnergyLevel
resetEnergyLevel Tired = Lev 0
resetEnergyLevel other = other

increaseCoordinateEnergyLevel :: Coordinate -> OctopusGrid -> OctopusGrid
increaseCoordinateEnergyLevel = M.adjust increaseEnergyLevel

deactivateCoordinateFlash :: Coordinate -> OctopusGrid -> OctopusGrid
deactivateCoordinateFlash = M.adjust deactivateFlash

increaseGridEnergyLevel :: OctopusGrid -> OctopusGrid
increaseGridEnergyLevel = M.map increaseEnergyLevel

propagateFlashEffect :: OctopusGrid -> OctopusGrid
-- must be a recursion
propagateFlashEffect g =
  if g == g_new then g else propagateFlashEffect g_new
  where
    -- find those who just flashed
    just_flashed = M.keys . M.filter (== Flash) $ g
    -- they are increasing the energy lev of their neighbors
    neighbors = concatMap adjacentCoordinates just_flashed
    g_1 = foldl (flip increaseCoordinateEnergyLevel) g neighbors
    -- and after that deactivate the flashes of current iteration
    g_2 = foldl (flip deactivateCoordinateFlash) g_1 just_flashed
    g_new = g_2

resetGridEnergyLevel :: OctopusGrid -> OctopusGrid
resetGridEnergyLevel = M.map resetEnergyLevel

countFlashTrails :: OctopusGrid -> Int
countFlashTrails = M.size . M.filter (== Lev 0)
