import Data.List
import Data.List.Split
import Debug.Trace
import System.IO

parseCrabs :: String -> [Int]
parseCrabs = map read . splitOn ","

bestLocation1 :: [Int] -> Int
bestLocation1 nums
  | even (length nums) = sorted !! i
  | otherwise = (sorted !! i + sorted !! (i + 1)) `div` 2
  where
    sorted = sort nums
    i = (length nums - 1) `div` 2

part1Ans :: [Int] -> Int
part1Ans nums = sum $ map (\x -> abs (x - mid)) nums
  where
    mid = bestLocation1 nums

bestLoc2 :: [Int] -> Int
bestLoc2 nums = round $ realToFrac (sum nums) / realToFrac (length nums)

cost2 mid nums = sum (map (dist mid) nums)
  where
    dist x y = abs (x - y) * (abs (x - y) + 1) `div` 2

part2Ans :: [Int] -> Int
part2Ans nums = minimum (map (`cost2` nums) [start .. end])
  where
    start = minimum nums
    end = maximum nums

main :: IO ()
main = do
  part1 <- (part1Ans . parseCrabs) . head . lines <$> readFile "input"
  putStr "Part 1: "
  print part1

  part2 <- (part2Ans . parseCrabs) . head . lines <$> readFile "input"
  putStr "Part 2: "
  print part2
