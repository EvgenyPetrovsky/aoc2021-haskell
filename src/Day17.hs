module Day17 (
  dayNo,
  part1,
  part2
) where

import qualified Lib
--import qualified Data.List as L
--import qualified Data.Map as M

dayNo = 17

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

newtype Input = Input Area
type Ordinate = Int
data Position = Position { xP :: Ordinate, yP :: Ordinate } deriving (Show, Eq)
type Trajectory = [Position]
data Speed = Speed {xS :: Ordinate, yS :: Ordinate} deriving (Show, Eq)
data Area = Area { xR :: (Ordinate, Ordinate), yR :: (Ordinate, Ordinate) } deriving (Show, Eq)

{- Solutions -}

parseInput :: [String] -> Input
parseInput _ =
  Input ( Area { xR = (79, 137), yR = (-176,-117) } )

{-
  it is important that
    a) probe ends up in valid x range
    b) probe ends up at lowest possible y value from valid y range
  both conditions are not affecting each other
  so we ignore the physics of x coordinate and focus on y

-}
solvePart_1 :: Solution
solvePart_1 (Input area) =
  height speed_at_0_level
  where
    min_y = negate $ uncurry min (yR area)
    speed_at_0_level = min_y - 1
    height :: Int -> Int
    height speed = if speed < 1 then 0 else speed + height (speed - 1)

{-
  find all speeds that produce trajectories that hit target area.
  use some guessing about speed ranges: 
    x is between 0 and max of Target Area x
    y is between - max abs y and + max abs y
  just build trajectories for product and pick those where any of positions belongs to target area
-}
solvePart_2 :: Solution
solvePart_2 (Input area) =
  length . filter (\s -> speedHitsTarget s area) $ speeds
  where 
    speeds = [Speed { xS = xs, yS = ys} | xs <- [0..max_x], ys <- [(negate max_y)..max_y]]
    max_x = max (fst . xR $ area) (snd . xR $ area)
    max_y = max (abs . fst . yR $ area) (abs . snd . yR $ area)


speedHitsTarget :: Speed -> Area -> Bool
speedHitsTarget s a = 
  any pointBelongsToArea t
  where 
    t = takeWhile (\p -> (yP p) >= y_min) $ trajectory s
    (x_min, x_max) = xR a
    (y_min, y_max) = yR a
    pointBelongsToArea :: Position -> Bool
    pointBelongsToArea Position {xP = x, yP = y} = 
      x_min <= x && x <= x_max && y_min <= y && y <= y_max
      


{- Supplementary functions -}

type Time = Int

trajectory :: Speed -> [Position]
trajectory initial_speed =
  zipWith speed_fun xs ys
  where
    speed_fun = \ x y -> Position { xP = x, yP = y }
    xs = xTrajectory initial_speed
    ys = yTrajectory initial_speed

xPosition :: Time -> Speed -> Ordinate
xPosition t s = xTrajectory s !! t

yPosition :: Time -> Speed -> Ordinate
yPosition t s = yTrajectory s !! t

xTrajectory :: Speed -> [Ordinate]
xTrajectory Speed { xS = initial_x_speed } = 
  scanl (+) initial_position speeds
  where 
    initial_position = 0
    speeds = reverse [0..initial_x_speed] ++ repeat 0

yTrajectory :: Speed -> [Ordinate]
yTrajectory Speed { yS = initial_y_speed } = 
  scanl (+) initial_position speeds
  where 
    initial_position = 0
    speeds = [initial_y_speed, (initial_y_speed - 1)..]
