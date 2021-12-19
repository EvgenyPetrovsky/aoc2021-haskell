module Day03 (
  part1,
  part2
) where

import qualified Lib as L

dayN = 3
type Input = [BinarySequence]
type BinarySequence = [Int]
type Answer = Int
type Solution = (Input -> Answer)

data ValueType = LeastCommon | MostCommon

printPartN :: Solution -> IO ()
printPartN solution = do
  input <- L.loadFile dayN
  let answer = solution . parseInput $ input
  putStr . show $ answer

part1 :: IO ()
part1 = printPartN solvePart_1

part2 :: IO ()
part2 = printPartN solvePart_2


-- Every line contains 0s and 1s that need to be presented as list of digits
parseInput :: [String] -> Input
parseInput = map (map (\c -> read [c] :: Int))

{-
process all commands to modify position of a submarine
-}
solvePart_1 :: Solution
solvePart_1 xs =
  powerConsumption
  where
    powerConsumption = gammaRate * epsilonRate
    gammaRate = binToDec . commonBits $ xs
    epsilonRate = binToDec . invert $ commonBits xs

invert :: BinarySequence -> BinarySequence
invert = map (1 - )

binToDec :: BinarySequence -> Int
binToDec = foldl (\z x -> 2 * z + x) 0

commonBits :: Input -> BinarySequence
commonBits xs =
  map (\s -> if s * 2 >= cnt then 1 else 0) bitSums
  where
    cnt = length xs
    bitSums = foldl1 (zipWith (+)) xs

{-
Process commands again using different structure
-}
solvePart_2 :: Solution
solvePart_2 xs =
  lifeSupportRating
  where
    lifeSupportRating = oxigenGeneratorRating * co2ScrubberRating
    oxigenGeneratorRating = binToDec $ find' MostCommon xs
    co2ScrubberRating = binToDec $ find' LeastCommon xs

-- use 1st parameter as a filter condition to find the best possible match
find' :: ValueType -> Input -> BinarySequence
find' valType input =
  iter 0 input
  where
    seqLen = length . head $ input
    iter :: Int -> Input -> BinarySequence
    iter pos input
      | length input == 1 = head input
      | pos >= seqLen = head input
      | otherwise =
        iter (pos + 1) $ filter (\x -> searchBit == x !! pos) input
      where
        searchBit = case valType of
          MostCommon -> commonBit
          LeastCommon -> invertBit
        commonBit = if count_1 * 2 >= length input then 1 else 0
        invertBit = 1 - commonBit
        count_1 = length . filter (\x -> x !! pos == 1) $ input
