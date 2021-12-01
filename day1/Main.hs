import System.IO

part1Ans :: [Int] -> Int
part1Ans [] = 0
part1Ans nums@(first : rest) = length $ filter id $ zipWith (>) rest nums

part2Ans :: [Int] -> Int
part2Ans nums@(first : second : rest) = part1Ans sliding
  where
    sliding = zipWith3 (\a b c -> a + b + c) nums (second : rest) rest
part2Ans _ = 0

main :: IO ()
main = do
  nums <- map read . lines <$> readFile "input"
  let part1 = part1Ans nums
  putStr "Part 1: "
  print part1
  let part2 = part2Ans nums
  putStr "Part 1: "
  print part2
