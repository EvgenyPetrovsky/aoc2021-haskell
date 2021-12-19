module Day05 (
  part1,
  part2
) where

import qualified Lib as L
import qualified Data.Map as DM
import qualified Data.Text as DT

dayN = 5
type Answer = Int
type Solution = (Input -> Answer)

data HTV = HTV { x :: Int, y :: Int } deriving (Eq, Ord, Show)

{- represent segments, ends are icluded -}
data HTVLine = HTVLine { from :: HTV, to :: HTV } deriving Show

newtype Input = Input [HTVLine]

type HTVMap = DM.Map HTV Int

printPartN :: Solution -> IO ()
printPartN solution = do
  input <- L.loadFile dayN
  let answer = solution . parseInput $ input
  putStr . show $ answer

part1 :: IO ()
part1 = printPartN solvePart_1

part2 :: IO ()
part2 = printPartN solvePart_2

parseInput :: [String] -> Input
parseInput xs = Input $ map parseLine xs

parseLine :: String -> HTVLine
parseLine ln =
  HTVLine {
    from = HTV { x = x1, y = y1},
    to = HTV { x = x2, y = y2}
  }
  where
    point_del = DT.pack " -> "
    coord_del = DT.pack ","
    (from_text:to_text:_) = DT.splitOn point_del (DT.pack ln)
    (x1:y1:_) = map (read . DT.unpack) . DT.splitOn coord_del $ from_text
    (x2:y2:_) = map (read . DT.unpack) . DT.splitOn coord_del $ to_text

{------------------------------------------------------------------------------}

{-
Consider only horizontal and vertical lines.
At how many points do at least two lines overlap?
-}
solvePart_1 :: Solution
solvePart_1 (Input i) =
  --DM.size . DM.filter (>= 2) . foldl updateHTVMap DM.empty $ pnts
  DM.size . DM.filter (>= 2) . foldl addHTVonMap DM.empty $ pnts
  where
    -- take only horizontal and vertical lines
    hzvt = filter (\x -> isHzLine x || isVtLine x) i
    -- get all points out of lines
    pnts = foldl1 (++) . map lineToPoints $ hzvt

isVtLine :: HTVLine -> Bool
isVtLine l = x (from l) == x (to l)

isHzLine :: HTVLine -> Bool
isHzLine l = y (from l) == y (to l)

isDgLine :: HTVLine -> Bool
isDgLine l = abs(x1 - x2) == abs(y1 - y2)
  where
    HTVLine{ from = HTV {x = x1, y = y1}, to = HTV {x = x2, y = y2} } = l

lineToPoints :: HTVLine -> [HTV]
lineToPoints l
  | isHzLine l = [ HTV{x = xs, y = y1} | xs <- xs_ ]
  | isVtLine l = [ HTV{x = x1, y = ys} | ys <- ys_]
  | isDgLine l = [ HTV{x = xs, y = ys} | (xs, ys) <- zip xs_ ys_ ]
  | otherwise = []
  where
    HTVLine{ from = HTV {x = x1, y = y1}, to = HTV {x = x2, y = y2} } = l
    xs_ = if x1 > x2 then reverse [x2..x1] else [x1..x2]
    ys_ = if y1 > y2 then reverse [y2..y1] else [y1..y2]

addHTVonMap :: HTVMap -> HTV -> HTVMap
addHTVonMap m p = DM.insertWith (+) p 1 m

solvePart_2 :: Solution
solvePart_2 (Input i) =
  --DM.size . DM.filter (>= 2) . foldl updateHTVMap DM.empty $ pnts
  DM.size . DM.filter (>= 2) . foldl addHTVonMap DM.empty $ pnts
  where
    -- get all points out of lines
    pnts = foldl1 (++) . map lineToPoints $ i
