import Data.Binary
import Data.Bits
import Data.Char (digitToInt)
import Data.List (foldl')
import Debug.Trace
import System.IO

toDec :: String -> Word
toDec s = fromIntegral (foldl' (\acc x -> acc * 2 + digitToInt x) 0 s)

mostCommonBit :: [Bool] -> Bool
mostCommonBit bits = oneCount >= zeroCount
  where
    (zeroCount, oneCount) = foldr countBit (0, 0) bits
    countBit True (a, b) = (a, b + 1)
    countBit False (a, b) = (a + 1, b)

gammaRate1 :: [Word] -> (Word, Int)
gammaRate1 nums = (fun zeroBits start nums, start)
  where
    start = maximum (map (\x -> finiteBitSize x - countLeadingZeros x) nums) - 1
    fun curr (-1) nums = curr
    fun curr b nums = fun next (b - 1) nums
      where
        mostCommon = mostCommonBit $ map (`testBit` b) nums
        next = if mostCommon then setBit curr b else curr

part1Ans :: [Word] -> Word
part1Ans nums = gamma * epsilon
  where
    (gamma, len) = gammaRate1 nums
    epsilon = flipBits len gamma
    flipBits (-1) b = b
    flipBits i b = flipBits (i - 1) (complementBit b i)

oxygen :: [Word] -> (Word, Int)
oxygen nums = (fun start nums, start)
  where
    start = maximum (map (\x -> finiteBitSize x - countLeadingZeros x) nums) - 1
    fun _ [] = error "Empty list!"
    fun b [num] = num
    fun b nums = fun (b - 1) filteredNums
      where
        mostCommon = mostCommonBit $ map (`testBit` b) nums
        filteredNums = filter (\x -> testBit x b == mostCommon) nums

co2 :: [Word] -> (Word, Int)
co2 nums = (fun start nums, start)
  where
    start = maximum (map (\x -> finiteBitSize x - countLeadingZeros x) nums) - 1
    fun _ [] = error "Empty list!"
    fun b [num] = num
    fun b nums = fun (b - 1) filteredNums
      where
        mostCommon = mostCommonBit $ map (`testBit` b) nums
        filteredNums = filter (\x -> testBit x b `xor` mostCommon) nums

part2Ans :: [Word] -> Word
part2Ans nums = o * co
  where
    (o, _) = oxygen nums
    (co, _) = co2 nums

main :: IO ()
main = do
  nums <- map toDec . lines <$> readFile "input"
  putStr "Part 1: "
  print $ part1Ans nums

  putStr "Part 2: "
  print $ part2Ans nums
