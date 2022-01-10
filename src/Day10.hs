module Day10 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
import qualified Data.List as L
import qualified Data.Map as M

dayNo = 10 :: Int

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

newtype Input = Input NavigationSubsystem

type NavigationSubsystem = [Line]
type Line = [Char]

--chunk
--open
--close
--matching characters

--incomplete
--corrupted --choses with wrong character

------

type MatchingPairs = M.Map Char Char

{- Solutions -}

parseInput :: [String] -> Input
parseInput = Input

solvePart_1 :: Solution
solvePart_1 (Input i) =
  sum . map (scoreCorrupted . head . reducePairs "") $ filter isCorrupted i

solvePart_2 :: Solution
solvePart_2 (Input i) =
  L.sort scores !! mid
  where
    mid = length scores `div` 2
    scores = map (scoreCompletion . reducePairs1 "") incomplete
    incomplete = filter (not . isCorrupted) i

{- Supplementary functions -}

matchingPairs :: MatchingPairs
matchingPairs = M.fromList[('(', ')'), ('[',']'), ('{', '}'), ('<', '>')]
openChars = M.keys matchingPairs
closeChars = M.elems matchingPairs

isCorrupted :: Line -> Bool
isCorrupted l = reducePairs "" l /= ""

reducePairs :: [Char] -> Line -> Line
reducePairs _ "" = ""
reducePairs [] (c:cs) = reducePairs [c] cs
reducePairs (o:os) (c:cs)
  | c `elem` openChars = reducePairs (c:o:os) cs
  | c == matchingPairs M.! o = reducePairs os cs
  | c `elem` closeChars = c:cs
  | otherwise = reducePairs (o:os) cs

reducePairs1 :: [Char] -> Line -> Line
reducePairs1 [] (c:cs) = reducePairs1 [c] cs
reducePairs1 (o:os) [] = o:os
reducePairs1 (o:os) (c:cs)
  | c `elem` openChars = reducePairs1 (c:o:os) cs
  | c == matchingPairs M.! o = reducePairs1 os cs
  | c `elem` closeChars = c:cs
  | otherwise = reducePairs (o:os) cs

scoreCorrupted :: Char -> Int
scoreCorrupted c = M.findWithDefault 0 c scores
  where
    scores = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

scoreCompletion :: [Char] -> Int
scoreCompletion =
  foldl (\z x -> z * 5 + scores M.! x) 0
  where
    scores = M.fromList $ "([{<" `zip` [1..4]