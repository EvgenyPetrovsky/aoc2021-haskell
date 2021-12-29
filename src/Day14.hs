module Day14 (
  dayNo,
  part1,
  part2

) where

import qualified Lib
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (pack, unpack, splitOn)

dayNo = 14

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

data Input = Input Polymer PolyRules
type Polymer = [Element]
type PolyRules = M.Map (Element, Element) Element
type Element = Char

type PolyPairs = M.Map (Element, Element) Int

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs =
  Input template rules
  where
    template = parseTemplate . head $ top_part
    rules = M.fromList . map parsePolyRule $ bottom_part
    top_part = take 1 xs
    bottom_part = drop (1 + length top_part) xs

parseTemplate :: String -> Polymer
parseTemplate = id

parsePolyRule :: String -> ((Element, Element), Element)
parsePolyRule s = ((l, r), n)
  where
    delimiter = pack " -> "
    (lrs:ns:_) = map unpack . splitOn delimiter $ pack s
    (l:r:_) = lrs
    (n:_) = ns

solvePart_1 :: Solution
solvePart_1 (Input template rules) =
  most_common_count - least_common_count
  where
    chain = foldl (\ z _ -> processChain z rules) template [1..10]
    groups = map length . L.group . L.sort $ chain
    most_common_count = maximum groups
    least_common_count = minimum groups

solvePart_2 :: Solution
solvePart_2 (Input template rules) =
  {- Part to requires another approach to run all 40 iterations -}
  {-

    The approach:
    - produce pairs of consequtive elements from templates and counts of recurrence
    - split those pairs which are in PolyRules
      if by splitting a pair existing pair is produced then quantities must be summed up
      do it as a map and not as a fold to avoid immediate effect of current iteration
    - transform pairs and recurrences into elements and recurrences and sum up non-unique elements

  -}
  (most_common_count - least_common_count) `div` 2
  where
    pairs_list :: [(Element, Element)]
    pairs_list = zip template (tail template)
    template_pairs :: PolyPairs
    template_pairs = foldl (\z x -> M.insertWith (+) x 1 z ) M.empty pairs_list
    poly_pairs = foldl (\ z _ -> processPairs z rules) template_pairs [1..40]
    elem_count = M.elems . splitPairsToElems $ poly_pairs
    most_common_count = maximum elem_count
    least_common_count = minimum elem_count

{- Supplementary functions -}

processChain :: Polymer -> PolyRules -> Polymer
processChain [r] _ = [r]
processChain (l:r:t) rules =
  case rules M.!? (l, r) of
    Just n -> l:n: processChain (r:t) rules
    _ -> l : processChain (r:t) rules

processPairs :: PolyPairs -> PolyRules -> PolyPairs
processPairs pairs rules =
  foldl (\z (k, a) -> M.insertWith (+) k a z) M.empty new_pairs
  where
    new_pairs :: [((Element, Element), Int)]
    new_pairs = concatMap (`split` rules) . M.toList $ pairs
    split :: ((Element, Element), Int) -> PolyRules -> [((Element, Element), Int)]
    split ((left, right), v) rules =
      case rules M.!? (left, right) of
        Just mid -> [((left, mid), v), ((mid, right), v)]
        _ -> [((left, right), v)]

splitPairsToElems :: PolyPairs -> M.Map Element Int
splitPairsToElems pairs =
  foldl (\z (k,v) -> M.insertWith (+) k v z) M.empty element_counts
  where
    element_counts = concatMap (\((l,r), v) -> [(l,v),(r,v)]) (M.toList pairs)