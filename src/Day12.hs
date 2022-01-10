module Day12 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
import qualified Data.List as L
--import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tuple (swap)

dayNo = 12

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

newtype Input = Input [Connection]
type Connection = (Point, Point)
data Point = Start | End | Big String | Small String
  deriving (Eq, Show, Ord)
type Path = [Point]
type Validation = Path -> Connection -> Bool

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  Input connections
  where
    delimiter = T.pack "-"
    splits = map (map T.unpack . T.splitOn delimiter . T.pack) xs
    connections = map (\(t1:t2:_) -> (parsePoint t1, parsePoint t2)) splits

parsePoint :: String -> Point
parsePoint "start" = Start
parsePoint "end" = End
parsePoint x
  | head x `elem` ['A'..'Z'] = Big x
  | head x `elem` ['a'..'z'] = Small x
  | otherwise = error $ "Unrecognized point " ++ x

allConnections :: [Connection] -> [Connection]
allConnections cs = cs ++ map swap cs


solvePart_1 :: Solution
solvePart_1 (Input i) =
  length (searchPath [Start] (allConnections i) validate_1)

solvePart_2 :: Solution
solvePart_2 (Input i) =
  length (searchPath [Start] (allConnections i) validate_2)

{- Supplementary functions -}

{-
this should be a recursive solution
before start flip

1 start in Start point
2 stop if current point is End
3 select all connections that have 1st element = current point
   and filter out those where second element is Small && visited
   and filter out those where second
4 build paths recursively for all selected connections

algorithm must be deterministic because
there are no Connections from Big to Big
-}

searchPath :: Path -> [Connection] -> Validation -> [Path]
searchPath (End:rest) _ _ = [End:rest]
searchPath path connections validate =
  -- search new paths for every path + valid next points
  concatMap
    ((\ p -> searchPath p connections validate)
       . (\ (_, b) -> b : path))
    valid_connections
  where
    current_point = head path
    compatible_connections = filter (\(a,_) -> current_point == a) connections
    valid_connections = filter (validate path) compatible_connections

validate_1 :: Validation
validate_1 path (_, point) =
  case point of
    Big _ -> True
    Small _ -> point `notElem` path
    End -> True
    _ -> False

validate_2 :: Validation
validate_2 path (_, point) =
  case point of
    Big _ -> True
    Small _ -> point `notElem` path || no_duplicates
    End -> True
    _ -> False
  where
    small_points = filter small path
    no_duplicates = 1 == (maximum . map length . L.group . L.sort $ small_points)
    small :: Point -> Bool
    small (Small _) = True
    small _ = False
