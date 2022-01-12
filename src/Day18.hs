module Day18 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
--import qualified Data.List as L
--import qualified Data.Map as M

dayNo = 18

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

newtype Input = Input [Number]

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs = Input (map parseLine xs)

parseLine :: String -> Number
parseLine (s:ss)
  | s `elem` ['0'..'9'] = Leaf . read $ [s]
  | s == '[' = Fork (parseLine left_part) (parseLine right_part)
    where
      (left_part, rest) = splitOnSameLevel ',' ss
      (right_part, _) = splitOnSameLevel ']' (tail rest)

splitOnSameLevel :: Char -> String -> (String, String)
splitOnSameLevel c s =
  splitAt pos s
  where
    pos = iter c s 0 0
    iter :: Char -> String -> Int -> Int -> Int
    iter c (s:ss) level acc
      | s == c && level == 0 = acc
      | s == '[' = iter c ss (level+1) (acc+1)
      | s == ']' = iter c ss (level-1) (acc+1)
      | otherwise = iter c ss level (acc+1)

solvePart_1 :: Solution
solvePart_1 (Input i) =
  error "not implemented"

solvePart_2 :: Solution
solvePart_2 (Input i) =
  error "not implemented"

{- Supplementary functions -}

data Number = Fork Number Number | Leaf Int
  deriving (Show, Eq)

data Explosion = Expolosion ExParticle | NoExplosion
data ExParticle = NPart | LPart Int | RPart Int

add :: Number -> Number -> Number
add p1 p2 = Fork p1 p2

{- reduce is a combination of explode and split operations -}
reduce :: Number -> Number
reduce = error "not implemented"

addParticle :: Number -> ExParticle -> Number
addParticle (Leaf n) p =
  case p of
    LPart x -> Leaf (n + x)
    RPart x -> Leaf (n + x)
    NPart -> Leaf n
addParticle (Fork l r) p =
  case p of
    LPart x -> Fork l (addParticle r p)
    RPart x -> Fork (addParticle l p) r
    NPart -> Fork l r

explode :: Number -> Number
explode = error "not implemented"

{-
explode' :: Int -> Number -> ExParticle -> (Number, Explosion)
-- if there is a particle
explode' level (Leaf n) p =
  case p of
    NPart -> (Leaf n, NoExplosion)
    LPart x -> (Leaf (n + x), Expolosion { l = NPart, r = NPart } )
    RPart x -> (Leaf (n + x), Expolosion { l = NPart, r = NPart } )
explode' level (Fork l r) NPart =
  if (isForkOfLeafs l) && level >= 4
  then (Fork (Leaf 0) (addParticle r RPart n1), Explosion LPart n1)
      else
    (Leaf l1, Leaf r1) -> if level >= 4 then (Fork l r, Explosion LPart l1 RPart r1)
    (Fork l1 r1, _) ->
      case (el,er) of
        (Explosion lparticle rparticle,_) -> (Fork l2 (fst $ explode' 0 r1 particle)
        (Explosion particle _,_) -> Fork l2 (explode' 0 r1 particle)
        where
          (l2, el) = explode' (level + 1) l1 NPart
          (r2, er) = explode' (level + 1) r1 NPart
-}

isForkOfLeafs :: Number -> Bool
isForkOfLeafs (Fork (Leaf _) (Leaf _)) = True
isForkOfLeafs _ = False

split :: Number -> Number
split n = res
  where (res, _) = split' n False

split' :: Number -> Bool -> (Number, Bool)

-- split leaf if nothing was split before and value is >= 10
split' (Leaf n) False =
  if n >= 10
  then (Fork l r, True)
  else (Leaf n, False)
  where
    l = Leaf d
    r = Leaf (d + m)
    (d,m) = n `divMod` 2
-- split fork: first left part and if there was no split, then right part
split' (Fork l r) False =
  case (lx,rx) of
    (True,_) -> (Fork l1 r, True)
    (_,True) -> (Fork l r1, True)
    _ -> (Fork l r, False)
  where
    (l1, lx) = split' l False
    (r1, rx) = split' r False
-- we don't split if something has been already split
split' n True = (n, True)
