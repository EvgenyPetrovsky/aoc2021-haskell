module Day09 (
  dayNo,
  part1,
  part2

) where

import qualified Lib as Lib
import qualified Data.List as L
import qualified Data.Map as M

dayNo = 09 :: Int

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

newtype Input = Input HeightMap
type HeightMap = [[Height]]
type Height = Int
type Coordinate = (Int, Int)
type AreaNumber = Int
type DiscoveredAreaMap = M.Map Coordinate AreaNumber

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  Input (map (map (\x -> read [x])) xs)

-- Heights of matrix
h :: HeightMap -> Int
h = length
-- Width of matrix
w :: HeightMap -> Int
w = length . head


solvePart_1 :: Solution
solvePart_1 (Input hm) =
  sum . map (riskLevel . c_Height) . filter isLowPointCoordinate $ cs
  where
    cs = allCoordinates hm
    isLowPointCoordinate :: Coordinate -> Bool
    isLowPointCoordinate c = isLowPoint (c_Height c) (adj_Heights c)
    adj_Heights :: Coordinate -> [Height]
    adj_Heights c = map c_Height $ adjacentCoordinates c hm
    c_Height :: Coordinate -> Height
    c_Height c = coordinateHeight c hm
    riskLevel = (+) 1

coordinateHeight :: Coordinate -> HeightMap -> Height
coordinateHeight (x, y) hm = hm !! y !! x

allCoordinates :: HeightMap -> [Coordinate]
allCoordinates hm =
  [(x, y) | x <- take hm_width [0..], y <- take hm_height [0..]]
  where
    hm_width = w hm
    hm_height = h hm

isLowPoint :: Height -> [Height] -> Bool
isLowPoint point_h adjacent_hs = point_h < minimum adjacent_hs

adjacentCoordinates :: Coordinate -> HeightMap -> [Coordinate]
adjacentCoordinates (x, y) hm =
   [(x+1, y), (x, y+1), (x-1, y), (x, y-1)] `L.intersect` allCoordinates hm

solvePart_2 :: Solution
solvePart_2 (Input hm) =
    product . take 3 . reverse . L.sort . map length . L.group . L.sort $ area_nums
  where
    area_nums = M.elems area
    area = foldl (\z x -> searchArea x z hm) M.empty (allCoordinates hm)

{-
given starting coordinate
AreaNumber =
  if coordinate is a border then skip
  if coordinate is marked on a DAM then use its AreaNumber,
  otherwise give it new AreaNumber = max + 1

find neighbor coordinates that are not borders and not yet discovered
mark them all with AreaNumber and run the same logic for all of them
-}

searchArea :: Coordinate -> DiscoveredAreaMap -> HeightMap -> DiscoveredAreaMap
searchArea c dam hm =
  if isBorderCoordinate c hm then dam
  else foldl (\z x -> searchArea x z hm) new_dam to_further_search
  where
    area_number = case dam M.!? c of
      Just x -> x
      Nothing -> M.foldl max 0 dam + 1
    new_dam = foldl (\z x -> M.insert x area_number z) dam to_further_search
    to_further_search = filter (\x -> isNotDiscovered x  && isNotBorder x) neighbors
    neighbors = adjacentCoordinates c hm
    isNotDiscovered c = c `notElem` M.keys dam
    isNotBorder c = not (isBorderCoordinate c hm)

isBorderCoordinate :: Coordinate -> HeightMap -> Bool
isBorderCoordinate c hm = 9 == coordinateHeight c hm
