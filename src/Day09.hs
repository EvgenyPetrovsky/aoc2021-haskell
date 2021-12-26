module Day09 (
  dayNo,
  part1,
  part2
  
) where

import qualified Lib as Lib
import qualified Data.List as L

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

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  Input (map (map (\x -> read [x])) xs)

-- Heights of matrix
h :: HeightMap -> Int
h hm = length hm
-- Width of matrix
w :: HeightMap -> Int
w hm = length . head $ hm


solvePart_1 :: Solution
solvePart_1 (Input hm) =
  sum . map riskLevel . map (c_Height) . filter (isLowPointCoordinate) $ cs
  where 
    cs = allCoordinates hm
    isLowPointCoordinate :: Coordinate -> Bool
    isLowPointCoordinate c = isLowPoint (c_Height c) (adj_Heights c)
    adj_Heights :: Coordinate -> [Height]
    adj_Heights c = map (c_Height) $ adjacentCoordinates c hm
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
  L.intersect [(x+1, y), (x, y+1), (x-1, y), (x, y-1)] (allCoordinates hm)



solvePart_2 :: Solution
solvePart_2 (Input i) = 
  error "not implemented"

{- Supplementary functions -}
