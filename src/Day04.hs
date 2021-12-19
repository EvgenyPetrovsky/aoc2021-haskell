module Day04 (
  part1,
  part2
) where

import qualified Lib as L
import qualified Data.List as DL

dayN = 4
type Answer = Int
type Solution = (Input -> Answer)

data Input = Input {
  luckyNumbers:: [Int],
  bingoBoards:: [BingoBoard]
}
type BingoBoard = [[Int]]

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
parseInput xs =
  Input {luckyNumbers = numbers, bingoBoards = boards}
  where
    lines :: [String]
    lines = filter (/= "") xs
    numbers = map read . splitUsing ',' . head $ lines
    boards :: [BingoBoard]
    boards = map (map (map read . words)) . chunkOf 5 . tail $ lines
    splitUsing :: Char -> String -> [String]
    splitUsing d =
      map (filter (/= d)) . DL.groupBy (\_ y -> y /= d)

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = h : chunkOf n t
  where (h, t) = splitAt n xs

isWinner :: [Int] -> BingoBoard -> Bool
isWinner numbers board =
  any lineMatch $ board ++ transposed
  where
    transposed = DL.transpose board
    lineMatch :: [Int] -> Bool
    lineMatch = all ( `elem` numbers)



{-
there are boards and numbers are played one by one until any of boards wins
-}
solvePart_1 :: Solution
solvePart_1 i =
    wn * (sum . unmarkedNumbers (take wp numbers) $ wb)
  where
    numbers = luckyNumbers i
    boards = bingoBoards i
    (wp, wb) = findWinner 5
    wn = numbers !! (wp - 1)
    findWinner :: Int -> (Int, BingoBoard)
    findWinner n =
      let
        ns = take n numbers
        winners = filter (isWinner ns) boards
      in
        if not (null winners) then (n, head winners)
        else findWinner (n + 1)


unmarkedNumbers :: [Int] -> BingoBoard -> [Int]
unmarkedNumbers ns b = filter (`notElem` ns) fb
  where
    fb = foldr1 (++) b

{-
process all commands to modify position of a submarine
-}
solvePart_2 :: Solution
solvePart_2 i =
  lastWinningNumber * sum (unmarkedNumbers numbersLastWinner lastWinner)
  where
    numbers = luckyNumbers i
    boards = bingoBoards i
    numbersToWin = map (takesToWin numbers) boards
    longest = case DL.elemIndex (maximum numbersToWin) numbersToWin of
      Just n -> n
      _ -> -1
    numbersLastWinner = take (maximum numbersToWin) numbers
    lastWinner = boards !! longest
    lastWinningNumber = last numbersLastWinner

takesToWin :: [Int] -> BingoBoard -> Int
takesToWin ns b =
  iter 5
  where
    iter pos = if isWinner (take pos ns) b then pos else iter (pos + 1)
