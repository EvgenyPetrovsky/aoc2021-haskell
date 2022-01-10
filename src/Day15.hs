module Day15 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
--import qualified Data.List as L
import qualified Data.Map as M

dayNo = 15

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

newtype Input = Input Field

--type Visited = M.Map Position (PathScore, Path)
type Visited = M.Map Position BestPath
data BestPath = BestPath { path :: Path, score :: Score } deriving (Eq, Ord, Show)
type Position = (Int, Int)
type PathScore = Score
type Score = Int
type Path = [Position]

type Field = [[Score]]

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs = Input $ map (map (\x -> read [x])) xs

{-
  look around and form a parth with total score for neighbor position
  check visited positions, if position was visited and path score is lower than before - use new path otherwiese reject current option
  do until current position is end
-}

solvePart_1 :: Solution
solvePart_1 (Input field) =
  score $ visits M.! finishPosition field
  where
    visits = expandSearch visit0 field [startPosition]

solvePart_2 :: Solution
solvePart_2 (Input field) =
  score $ visits M.! finishPosition scaled
  where
    scaled = scaleField 5 field
    visits = expandSearch visit0 scaled [startPosition]

{- Supplementary functions -}

visit0 :: Visited
visit0 =
  M.singleton start_position $ BestPath { path = [start_position], score = 0}
  where
    start_position = startPosition

positionScore :: Position -> Field -> Score
positionScore (r,c) f = f !! r !! c

startPosition :: Position
startPosition = (0,0)

finishPosition :: Field -> Position
finishPosition f = (r,c)
  where
    r = length f - 1
    c = length (head f) - 1

adjacentPositions :: Position -> Field -> [Position]
adjacentPositions (r0,c0) f =
  [ (r,c) | r <- [r0-1..r0+1], c <- [c0-1..c0+1],
            c == c0 || r == r0, (c,r) /= (c0,r0),
            c >= min_c && c <= max_c,
            r >= min_r && r <= max_r]
  where
    (max_r, max_c) = finishPosition f
    (min_r, min_c) = startPosition

pathScore :: Path -> Field -> PathScore
pathScore p f = full_score - startPositionScore
  where
    full_score = sum . map (`positionScore` f) $ p
    startPositionScore = startPosition `positionScore` f

{- the main exploration algorithm -}
{-
  Take exploration restuls and analyse every position from it by
  - start with positions to check
  - obtaining coordinates of neighbor positions
  - build path for every neighbor coordinnate (by adding it to path of current position)
  - caculate path score and compare it with previously explored:
    - if that new position was not explored: add it into the map of visited plaecs
      if that new position was explored but previous path scoe is greater then new - add new into map of visited places
      otherwise keep map of visited places as it is
  - compare result in the end of iteration and in the beginning of iteration
    and pick all positions where result sis different for next iteration
-}
expandSearch :: Visited -> Field -> [Position] -> Visited
expandSearch vs f ps_to_check =
  if null ps_to_check then vs else expandSearch new_vs f new_ps_to_check
  where
    new_vs = foldl (exploreAdjacentPositions f) vs ps_to_check
    new_ps_to_check = filter (\x -> vs M.!? x /= new_vs M.!? x) $ M.keys new_vs

{-
  Get field data and map of Visited places and position
  and try find better paths to adjacent positions
-}
exploreAdjacentPositions :: Field -> Visited -> Position -> Visited
exploreAdjacentPositions f visits position =
  foldl (updateVisited f) visits paths
  where
    paths = map ( : path_to_position) $ adjacentPositions position f
    path_to_position = path ( visits M.! position )

{-
  if score of proposed path is lowqet than one registered in Map of Visited places
  or the position is not in a Map
  then add the positions with new path
  otherwise keep map as it is
-}
updateVisited :: Field -> Visited -> Path -> Visited
updateVisited field visits new_path =
  case visits M.!? position of
    Nothing -> M.insert position new_data visits
    Just BestPath { path = _, score = old_score } ->
      if old_score > new_score
      then M.update (\ _ -> Just new_data) position visits
      else visits
  where
    position = head new_path
    new_score = pathScore new_path field
    new_data = BestPath { path = new_path, score = new_score}

{-
  Scale field by factor of 5
  stack field five times to the right, increasing risk values of every consequtive copy by 1
  stack field five times down, increasing risk values of every consequtive copy by 1
-}

scaleField :: Int -> Field -> Field
scaleField n = scaleFieldDown n . scaleFieldRight n

scaleFieldRight :: Int -> Field -> Field
scaleFieldRight n f =
  foldl (\z x -> zipWith (++) z (increaseFieldScore x f)) f [1..(n-1)]

scaleFieldDown :: Int -> Field -> Field
scaleFieldDown n f =
  foldl (\z x -> z ++ increaseFieldScore x f) f [1..(n-1)]


increaseFieldScore :: Int -> Field -> Field
increaseFieldScore n = map (map increase)
  where
    increase :: Score -> Score
    increase x = let (d,m) = (x + n) `divMod` 10 in d + m

