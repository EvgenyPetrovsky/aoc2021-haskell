module Day18 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
--import qualified Data.List as L
--import qualified Data.Map as M

dayNo = 18

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

data Pair = 
  L Pair Int |
  R Int Pair |
  B Pair Pair |
  N Int Int
  deriving (Show, Eq)

add :: Pair -> Pair -> Pair
add p1 p2 = Pair p1 p2

reduce :: Pair -> Pair
reduce = iter
  where 

    iter :: Int -> Pair -> Pair -> Pair
    iter n l r = case (nPair l, nPair r)
      (True, True) -> if n < 4
nPair :: Pair -> Bool
nPair (N _ _) = True
nPair _ = False

lPair :: Pair -> Bool
lPair (L _ _) = True
lPair _ = False

rPair :: Pair -> Bool
rPair (R _ _) = True
rPair _ = False

bPair :: Pair -> Bool
bPair (B _ _) = True
bPair _ = False


explode :: Pair -> Pair
explode = error "not implemented"

split :: Pair -> Pair
split = error "not implemented"
