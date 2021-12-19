module Day06 (
  part1,
  part2
) where

import qualified Lib as L
import Data.Text (unpack, pack, splitOn)

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
type Population = [LanternFishBirthEpoch]

firstCycleAddon = 2 :: Int
cycleDuration = 7
epochs = [0..80] :: [Int]

newtype Input = Input Population

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs = Input pop
  where
    pop = map (read . unpack) . splitOn del . pack $ head xs
    del = pack ","


solvePart_1 :: Solution
solvePart_1 (Input i) =
  length . foldl createFish i $ epochs

createFish :: Population -> Epoch -> Population
createFish population epoch = population ++ newBorn
  where
    newBorn = map (const epoch) . filter (ageToCreate epoch) $ population

ageToCreate :: Epoch -> LanternFishBirthEpoch -> Bool
ageToCreate c b = (c - b - firstCycleAddon) `rem` cycleDuration == 0

solvePart_2 :: Solution
solvePart_2 (Input i) = 2
