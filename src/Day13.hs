module Day13 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
--import qualified Data.List as L
--import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

dayNo = 13

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

data Input = Input Image [Operation]
  deriving Show
type Image = S.Set Dot
type Dot = (Int, Int)
data Operation = FoldAlongX Int | FoldAlongY Int
  deriving (Eq, Ord, Show)

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  Input image operations
  where
    image = S.fromList . map parseDot $ top_part
    operations = map parseOperation bottom_part
    top_part = takeWhile (not . null) xs
    bottom_part = drop (1 + length top_part) xs

parseDot :: String -> Dot
parseDot s = (x,y)
  where
    delimiter = T.pack ","
    (x:y:_) = map (read . T.unpack) . T.splitOn delimiter $ T.pack s

parseOperation :: String -> Operation
parseOperation s =
  case op of
    "fold along x" -> FoldAlongX n
    "fold along y" -> FoldAlongY n
    _ -> error $ "unrecognized operation " ++ op
  where
    op = take 12 s
    n = read . drop 13 $ s

solvePart_1 :: Solution
solvePart_1 (Input dots ops) =
  S.size . foldl (flip apply) dots $ take 1 ops

solvePart_2 :: Solution
solvePart_2 (Input dots ops) =
  S.size . foldl (flip apply) dots $ ops
  -- CJHAZHKU

{- Supplementary functions -}

apply :: Operation -> Image -> Image
apply op dots =
  case op of
    FoldAlongX x -> foldAlongX x dots
    FoldAlongY y -> foldAlongY y dots

transformOrdinate :: Int -> Int -> Int
transformOrdinate mid num =
  if num <= mid then num else 2 * mid - num

foldAlongY :: Int -> Image -> Image
foldAlongY y = S.map transformY . S.filter (\ (_, fl) -> fl /= y)
  where
    transformY :: Dot -> Dot
    transformY (x0,y0) = (x0, transformOrdinate y y0)

foldAlongX :: Int -> Image -> Image
foldAlongX x = S.map transformX . S.filter (\ (fl, _) -> fl /= x)
  where
    transformX :: Dot -> Dot
    transformX (x0,y0) = (transformOrdinate x x0, y0)
