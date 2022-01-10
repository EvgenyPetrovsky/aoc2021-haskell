module Day16 (
  dayNo,
  part1,
  part2
) where

import qualified Lib
--import qualified Data.List as L
import qualified Data.Map as M

dayNo = 16

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

newtype Input = Input BitString

type BitString = [Bit]

data Bit = B0 | B1 deriving (Show, Eq, Ord)
data Type = TL | TO deriving (Show, Eq, Ord)
data LengthTypeID = I0 | I1 deriving (Show, Eq, Ord)
data Length = Subpackets Int | Subsequence Int deriving (Show, Eq, Ord)
data PacketFun = FSum | FProd | FMin | FMax | FGt | FLt | FEq | FUnknown
  deriving (Show, Eq, Ord)
data Packet =
  Li { version :: Int, type_id :: Int, value :: Int } |
  Op { version :: Int, type_id :: Int, subpackets :: [Packet], fun :: PacketFun }
  deriving (Eq, Show)

{- Solutions -}

parseInput :: [String] -> Input
parseInput xs = Input (concatMap hex2bit . head $ xs)

solvePart_1 :: Solution
solvePart_1 (Input i) =
    iter p
  where
    (p,_) = parsePacket i
    iter :: Packet -> Int
    iter Li {version = v, type_id = _, value = _} = v
    iter Op {version = v, type_id = _, subpackets = ps} = v + (sum . map iter $ ps)

solvePart_2 :: Solution
solvePart_2 (Input i) =
  calculate . fst . parsePacket $ i
  where
    calculate :: Packet -> Int
    calculate Li {value = x} = x
    calculate Op {fun = pf, subpackets = ps} = opFun pf $ map calculate ps


{- Supplementary functions -}

hex2bit :: Char -> [Bit]
hex2bit c = map (b M.!) $ m M.! c
  where
    m = M.fromList [
      ('0', "0000"), ('1', "0001"), ('2', "0010"), ('3', "0011"),
      ('4', "0100"), ('5', "0101"), ('6', "0110"), ('7', "0111"),
      ('8', "1000"), ('9', "1001"), ('A', "1010"), ('B', "1011"),
      ('C', "1100"), ('D', "1101"), ('E', "1110"), ('F', "1111")]
    b = M.fromList [('0', B0), ('1', B1)]

bits2dec :: [Bit] -> Int
bits2dec = foldl (\ z x -> z * 2 + bit2bin x) 0

bit2bin :: Bit -> Int
bit2bin B0 = 0
bit2bin B1 = 1

parsePacket :: BitString -> (Packet, BitString)
parsePacket bs =
    case t of
      TL -> (Li {version = v, type_id = t_id, value = li_val}, rb_Li)
      TO -> (Op {version = v, type_id = t_id, subpackets = subps, fun = f}, rb_Op)
  where
    -- version
    (b_V,rb_V) = splitAt 3 bs
    v = bits2dec b_V
    -- type
    (b_T,rb_T) = splitAt 3 rb_V
    t_id = bits2dec b_T
    t = case t_id of
      4 -> TL
      _ -> TO
    f = case t_id of
      0 -> FSum
      1 -> FProd
      2 -> FMin
      3 -> FMax
      5 -> FGt
      6 -> FLt
      7 -> FEq
      _ -> FUnknown

    -- literal value
    (li_val, rb_Li) =
      case t of
        TL -> parseLiteral rb_T
        TO -> (0, rb_T)
    -- length type id
    (b_I,rb_I) =
      case t of
        TL -> splitAt 0 rb_T
        TO -> splitAt 1 rb_T
    i = case bits2dec b_I of
      0 -> Just I0
      1 -> Just I1
      _ -> Nothing
    -- length
    (b_L, rb_L) = case i of
      Just I0 -> splitAt 15 rb_I
      Just I1 -> splitAt 11 rb_I
      Nothing -> splitAt 0 rb_I
    l =
      case i of
        Just I0 -> Subsequence $ bits2dec b_L
        Just I1 -> Subpackets $ bits2dec b_L
    -- subpackets
    (subps, rb_Op) =
      case l of
        Subpackets n -> parseSubPackets n rb_L
        Subsequence n -> parseSubSequence n rb_L

-- for operator packet extract list of subpackets (by length) and return rest
parseSubPackets :: Int -> BitString -> ([Packet], BitString)
parseSubPackets n bs =
  iter [] n bs
  where
    iter :: [Packet] -> Int -> BitString -> ([Packet], BitString)
    iter ps 0 bs = (reverse ps, bs)
    iter ps n bs = iter (p:ps) (n - 1) rb
      where (p, rb) = parsePacket bs

-- for operator packet extract list of subpackets (by count) and return the rest
parseSubSequence :: Int -> BitString -> ([Packet], BitString)
parseSubSequence n bs =
  (iter [] ss, rb)
  where
  (ss,rb) = splitAt n bs
  iter :: [Packet] -> BitString -> [Packet]
  iter ps [] = reverse ps
  iter ps bs = iter (p:ps) rb
    where (p, rb) = parsePacket bs

-- for literal packet extract numeric value and return the rest
parseLiteral :: BitString -> (Int, BitString)
parseLiteral bs =
  (bits2dec lb, rb)
  where
    (lb, rb) = iter [] bs
    iter acc (h:tl) =
      case h of
        B0 -> (acc ++ take 4 tl, drop 4 tl)
        B1 -> iter (acc ++ take 4 tl) (drop 4 tl)

-- operator functions
opFun :: PacketFun -> ([Int] -> Int)
opFun FSum = sum
opFun FProd = product
opFun FMin = minimum
opFun FMax = maximum
opFun FGt = \ (a:b:_) -> if a > b then 1 else 0
opFun FLt = \ (a:b:_) -> if a < b then 1 else 0
opFun FEq = \ (a:b:_) -> if a == b then 1 else 0
