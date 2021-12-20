module Day06 (
  part1,
  part2
) where

import qualified Lib as L
import Data.Text (unpack, pack, splitOn)
import qualified Data.Map as M

dayN = 6
type Answer = Int
type Solution = (Input -> Answer)

printPartN :: Solution -> IO ()
printPartN solution = do
  input <- L.loadFile dayN
  let answer = solution . parseInput $ input
  putStr . show $ answer

part1 :: IO ()
part1 = printPartN solvePart_1

part2 :: IO ()
part2 = printPartN solvePart_2

{- Define data types -}

type Epoch = Int
type LanternFishBirthEpoch = Epoch
type FishCount = Int
type Population = M.Map LanternFishBirthEpoch FishCount

firstCycleAddon = 2 :: Int
cycleDuration = 7 :: Int

newtype Input = Input Population

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  Input (foldl (\z e -> M.insertWith (+) e 1 z) M.empty epochs)
  where
    del = pack ","
    timers = map (read . unpack) . splitOn del . pack $ head xs
    epochs = map (\x -> x - cycleDuration - firstCycleAddon) timers


solvePart_1 :: Solution
solvePart_1 (Input i) =
  M.foldl (+) 0 . foldl createFish i $ epochs
  where 
    epochs = [1..80] :: [Int]


-- check population and find those generations that are ready to create
-- count number of fish of those generations and add new generation of this size
createFish :: Population -> Epoch -> Population
createFish population epoch = M.insert (epoch - 1) newborns population
  where
    newborns = M.foldl (+) 0 generations
    generations = M.filterWithKey (\k _ -> ageToCreate epoch k) population

ageToCreate :: Epoch -> LanternFishBirthEpoch -> Bool
ageToCreate e b =
  condition1 && condition2
  where
    condition1 = (e - b - firstCycleAddon) `rem` cycleDuration == 1
    condition2 = e - b - firstCycleAddon > 1

solvePart_2 :: Solution
solvePart_2 (Input i) = 
  M.foldl (+) 0 . foldl createFish i $ epochs
  where 
    epochs = [1..256] :: [Int]
