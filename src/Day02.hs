module Day02 (
  part1,
  part2
) where

import qualified Lib as L

data Instruction = Forward Int | Down Int | Up Int | Unknown deriving Show

data Position = Position Int Int
type Aim = Int
type PosX = Int
type PosY = Int
data State = State {
  aim :: Aim,
  x :: PosX,
  y :: PosY
} deriving Show

dayN = 2 :: Int
type Input = [Instruction]
type Answer = Int
type Solution = (Input -> Answer)

printPartN :: Solution -> IO ()
printPartN solution = do
  input <- L.loadFile dayN
  --putStrLn (unlines . map show . parseInput $ input)
  let answer = solution . parseInput $ input
  putStr . show $ answer

part1 :: IO ()
part1 = printPartN solvePart_1

part2 :: IO ()
part2 = printPartN solvePart_2


parseInput :: [String] -> Input
parseInput = map (parseLineItems . words)

parseLineItems :: [String] -> Instruction
parseLineItems ("forward":n:_) = Forward (read n)
parseLineItems ("up":n:_) = Up (read n :: Int)
parseLineItems ("down":n:_) = Down (read n :: Int)
parseLineItems _ = Unknown

{-
process all commands to modify position of a submarine
-}
solvePart_1 :: Solution
solvePart_1 = product . foldl move startPosition
  where
    startPosition = Position 0 0
    product (Position x y) = x * y

move :: Position -> Instruction -> Position
move (Position x y) i =
  secureDepth $ Position (x + dx) (y + dy)
  where
    (dx, dy) = step i
    secureDepth (Position x y) =
      if y < 0 then Position x 0 else Position x y


step :: Instruction -> (Int, Int)
step (Forward n) = (n, 0)
step (Down n) = (0, n)
step (Up n) = (0, negate n)
step _ = (0, 0)


{-
Process commands again using different structure
-}
solvePart_2 :: Solution
solvePart_2 = prod . foldl step2 startState
  where
    startState = State {aim = 0, x = 0, y = 0}
    prod s = x s * y s

step2 :: State -> Instruction -> State
step2 s (Down n) = State {aim = aim s + n, x = x s, y = y s}
step2 s (Up n) = State {aim = aim s - n, x = x s, y = y s}
step2 s (Forward n) = State {aim = aim s, x = x s + n, y = y s + (aim s * n)}
step2 s _ = s