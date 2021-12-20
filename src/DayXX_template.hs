module DayXX_template (
  dayNo,
  part1,
  part2
  
) where

import qualified Lib as L

dayNo = 00

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

newtype Input = Input [Int]

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  error "not implemented"

solvePart_1 :: Solution
solvePart_1 (Input i) =
  error "not implemented"

solvePart_2 :: Solution
solvePart_2 (Input i) = 
  error "not implemented"

{- Supplementary functions -}
