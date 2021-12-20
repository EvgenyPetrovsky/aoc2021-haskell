module Day08 (
  dayNo,
  part1,
  part2

) where

import qualified Lib as L
import qualified Data.Map as M
import qualified Data.Set as S

dayNo = 8

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

type Signal = S.Set Char

newtype Input = Input [Entry]
type Determinant = M.Map Signal Int

data Entry = Entry { signals :: [Signal], display :: [Signal] }

{- Solutions -}

parseInput :: [String] -> Input
parseInput ls = Input (map parseLine ls)

parseLine :: String -> Entry
parseLine l =
  Entry {signals = digits, display = display}
  where
    digits = map S.fromList . take 10 . words $ l
    display = map S.fromList . drop 11 . words $ l

solvePart_1 :: Solution
solvePart_1 (Input i) =
  sum . map length $ only1748s
  where
    displayed :: [[Signal]]
    displayed = map display i
    is1748 :: Signal -> Bool
    is1748 x = S.size x `elem` [2,3,4,7]
    only1748s :: [[Signal]]
    only1748s = map (filter is1748) displayed

solvePart_2 :: Solution
solvePart_2 (Input i) =
  error "not implemented"


determine :: [Signal] -> Determinant
determine signals =
  M.fromList $ zip [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] [0..]
  where
    s_0123456789 :: S.Set (S.Set Char)
    s_0123456789 = S.fromList signals
    d1 = S.elemAt 1 . S.filter (\x -> S.size x == 2) $ s_0123456789
    d7 = S.elemAt 1 . S.filter (\x -> S.size x == 3) $ s_0123456789
    d4 = S.elemAt 1 . S.filter (\x -> S.size x == 4) $ s_0123456789
    d8 = S.elemAt 1 . S.filter (\x -> S.size x == 7) $ s_0123456789
    s_023569 = foldl (flip S.delete) s_0123456789 [d1,d4,d7,d8]
    -- digit 3 has 5 segments and is superset of digit 7
    d3 = S.elemAt 1 . S.filter (\x -> S.intersection x d7 == d7 && S.size x == 5) $ s_023569
    s_02569 = S.delete d3 s_023569
    -- digit 9 has 6 segments and is superset of digit 3
    d9 = S.elemAt 1 . S.filter(\x -> S.intersection x d3 == d3 && S.size x == 6) $ s_02569
    s_0256 = S.delete d9 s_02569
    -- digit 0 has 6 segments and is superset of 7
    d0 = S.elemAt 1 . S.filter(\x -> S.intersection x d7 == d7 && S.size x == 6) $ s_0256
    s_256 = S.delete d0 s_0256
    -- digit 6 is the only remaining digit with 6 segments
    d6 = S.elemAt 1 . S.filter(\x -> S.size x == 6) $ s_256
    s_25 = S.delete d6 s_256
    -- digit 5 has 5 segments and is subset of 6
    d5 = S.elemAt 1 . S.filter(\x -> S.intersection x d6 == x && S.size x == 5) $ s_25
    s_2 = S.delete d5 s_25
    -- digit 2 is the only remaining digit
    d2 = S.elemAt 1 s_2

decode :: [Signal] -> Determinant -> Int
decode ss d =
  foldl (\z x -> z * 10 + x) 0 . map (d M.!) $ ss

{- Supplementary functions -}
