import Data.List.Split
import Debug.Trace
import System.IO

main :: IO ()
main = do
  part1 <- numAfterDays 80 . parseLantern <$> readFile "input"
  putStr "Part 1: "
  print part1

  part2 <- part2Ans 256 . parseSchool <$> readFile "input"
  putStr "Part 2: "
  print part2
