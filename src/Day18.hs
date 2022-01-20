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
parseInput xs = Input (map readNumber xs)

readNumber :: String -> Number
readNumber (s:ss)
  | s `elem` ['0'..'9'] = Leaf . read $ s:ss
  | s == '[' = Fork (readNumber left_part) (readNumber right_part)
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

showNumber :: Number -> String
showNumber (Leaf n) = show n
showNumber (Fork l r) = "[" ++ showNumber l ++ "," ++ showNumber r ++ "]"

solvePart_1 :: Solution
solvePart_1 (Input numbers) =
  magnitude new_number
  where
    new_number = foldl1 (\z x -> reduce (add z x)) numbers

magnitude :: Number -> Int
magnitude (Leaf n) = n
magnitude (Fork l r) = (3 * magnitude l) + (2 * magnitude r)

solvePart_2 :: Solution
solvePart_2 (Input numbers) =
  maximum . map add_red_mag $ cbs_all
  where
    cbs :: [[Number]]
    cbs = (combinations 2 numbers)
    cbs_all = cbs ++ reverse cbs
    add_red_mag :: [Number] -> Int
    add_red_mag = magnitude . foldl1 (\z x -> reduce (z `add` x))

{- Supplementary functions -}

data Number = Fork Number Number | Leaf Int
  deriving (Show, Eq)

data Explosion = Explosion ExParticle | NoExplosion
  deriving (Eq, Show)

data ExParticle = NPart | LPart Int | RPart Int
  deriving (Eq, Show)


add :: Number -> Number -> Number
add = Fork

{- reduce is a combination of explode and split operations -}
reduce :: Number -> Number
reduce n
  | n /= n_exp = reduce n_exp
  | n /= n_spl = reduce n_spl
  | otherwise = n
  where
    n_exp = explode n
    n_spl = split n
--reduce n =
--  if n == n_new then n else reduce n_new
--  where n_new = split . explode $ n

explode :: Number -> Number
explode n = new_n
  where
  (new_n,_) = explode' 1 n NoExplosion

explode' :: Int -> Number -> Explosion -> (Number, Explosion)

-- on the level of exact numbers we either add explosion part or do nothing
explode' _ (Leaf n) p =
  case p of
    NoExplosion -> (Leaf n, NoExplosion)
    Explosion NPart -> (Leaf n, Explosion NPart)
    Explosion (LPart x) -> (Leaf (n + x), Explosion NPart )
    Explosion (RPart x) -> (Leaf (n + x), Explosion NPart )

-- if explosion has happened and we know what part to allocate
-- then we give it to respective side of fork
-- do not analyse anything in depths - the task is allocation
explode' level (Fork l r) (Explosion p) =
  case p of
    (LPart _) -> (Fork l new_r, new_r_p)
    (RPart _) -> (Fork new_l r, new_l_p)
    _ -> (Fork l r, Explosion p)
  where
    (new_l, new_l_p) = explode' (level+1) l (Explosion p)
    (new_r, new_r_p) = explode' (level+1) r (Explosion p)

-- if explosion did not yet happen
-- then we need to check whether left part is pair of Regular Numbers and their level >=4 (current level >= 4)
--   if so,
--     replace left part wirth regular 0 and allocate right part of explosion immediately to right element
--     and return new right element, new left element and new explosion (with left part from exploded pair)
--   if not so act recursively on left element and see if resuting Explosion changed
-- then we need to check whether right part is pair of Regular Numbers and their level >=4 (current level >= 4)
--   if so, replace right part wirth regular 0 and allocate right part of explosion immediately to left element
--   and return new left element, new right element and new explosion (with left part from exploded pair)
--
explode' level (Fork l r) NoExplosion
  | isForkOfLeafs l && level >= 4 =
    let
      in_e = Explosion (RPart l_leaf_r)
      (new_r,_) = explode' (level+1) r in_e
      up_e = Explosion (LPart l_leaf_l)
    in (Fork (Leaf 0) new_r, up_e)
  | new_l_expl /= NoExplosion =
    case new_l_expl of
      Explosion (LPart _) -> (Fork new_l r, new_l_expl)
      Explosion (RPart _) -> explRnFork level new_l r new_l_expl
      _ -> (Fork new_l r, new_l_expl)
  | isForkOfLeafs r && level >= 4 =
    let
      in_e = Explosion (LPart r_leaf_l)
      (new_l,_) = explode' (level+1) l in_e
      up_e = Explosion (RPart r_leaf_r)
    in (Fork new_l (Leaf 0), up_e)
  | otherwise =
    case new_r_expl of
      Explosion (RPart _) -> (Fork l new_r, new_r_expl)
      Explosion (LPart _) -> explLnFork level l new_r new_r_expl
      _ -> (Fork l new_r, new_r_expl)

  where
    (Fork (Leaf l_leaf_l) (Leaf l_leaf_r)) = l
    (Fork (Leaf r_leaf_l) (Leaf r_leaf_r)) = r
    (new_l, new_l_expl) = explode' (level+1) l NoExplosion
    (new_r, new_r_expl) = explode' (level+1) r NoExplosion
    explRnFork :: Int -> Number -> Number -> Explosion -> (Number, Explosion)
    explRnFork level l r e = (Fork l r1, e1)
      where (r1, e1) = explode' (level+1) r e
    explLnFork :: Int -> Number -> Number -> Explosion -> (Number, Explosion)
    explLnFork level l r e = (Fork l1 r, e1)
      where (l1, e1) = explode' (level+1) l e

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

-- part 2 ----------------------------------------------------------------------
-- combinations of n elements out of list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- combinations (n-1) (drop (i+1) xs) ]
                                  