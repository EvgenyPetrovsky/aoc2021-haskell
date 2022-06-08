module DayXX_template (
  dayNo,
  part1,
  part2

) where

import qualified Lib
import qualified Data.List as L
import qualified Data.Set as S

dayNo = 19

type Answer = Int
type Solution = (Input -> Answer)

type Scanner = BeaconMap
type BeaconMap = [Coordinate]
type Coordinate = (Int, Int, Int)

isNeighbourScanner :: Scanner -> Scanner -> Boolean


rotateCoordinates :: Coordinate -> [Coordinate]
rotateCoordinates (x, y, z) =
  -- first 4 - rotation around Z, last 2 - rotation around X
  concatMap rotate_around_y xs_rotations
  xz_rotations = ( x, y, z):(-y, x, z):(-x,-y, z):( y,-x, z):( x, z, -y):( x, -z, y):[]
  where 
    rotate_around_y :: Coordinate -> [Coordinate]
    rotate_around_y ( x, y, z) = ( z, y,-x):( z, y,-x):(-x, y,-z):(-z, y, x):[]

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
