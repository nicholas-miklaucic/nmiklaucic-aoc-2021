import Debug.Trace
import System.IO

type Lantern = Int

step :: Lantern -> [Lantern]
step 0 = [6, 8]
step n = [n - 1]

stepAll :: [Lantern] -> [Lantern]
stepAll = concatMap step

stepDays :: Int -> [Lantern] -> [Lantern]
stepDays n fish = iterate stepAll fish !! n

numAfterDays :: Int -> [Lantern] -> Int
numAfterDays n fish = length (stepDays n fish)

parseLantern :: String -> [Lantern]
parseLantern (n : ',' : rest) = read [n] : parseLantern rest
parseLantern [n, '\n'] = [read [n]]
parseLantern n = [read n]

data School = School Int Int Int Int Int Int Int Int Int

stepSchool :: School -> School
stepSchool (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n1 n2 n3 n4 n5 n6 (n7 + n0) n8 n0

addToSchool :: Lantern -> School -> School
addToSchool 0 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School (n0 + 1) n1 n2 n3 n4 n5 n6 n7 n8
addToSchool 1 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 (n1 + 1) n2 n3 n4 n5 n6 n7 n8
addToSchool 2 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 n1 (n2 + 1) n3 n4 n5 n6 n7 n8
addToSchool 3 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 n1 n2 (n3 + 1) n4 n5 n6 n7 n8
addToSchool 4 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 n1 n2 n3 (n4 + 1) n5 n6 n7 n8
addToSchool 5 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 n1 n2 n3 n4 (n5 + 1) n6 n7 n8
addToSchool 6 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 n1 n2 n3 n4 n5 (n6 + 1) n7 n8
addToSchool 7 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 n1 n2 n3 n4 n5 n6 (n7 + 1) n8
addToSchool 8 (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = School n0 n1 n2 n3 n4 n5 n6 n7 (n8 + 1)
addToSchool _ _school = error "Invalid lantern"

parseSchool :: String -> School
parseSchool s = foldr addToSchool (School 0 0 0 0 0 0 0 0 0) $ parseLantern s

numFish :: School -> Int
numFish (School n0 n1 n2 n3 n4 n5 n6 n7 n8) = n0 + n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8

part2Ans :: Int -> School -> Int
part2Ans n school = numFish $ iterate stepSchool school !! n

main :: IO ()
main = do
  part1 <- numAfterDays 80 . parseLantern <$> readFile "input"
  putStr "Part 1: "
  print part1

  part2 <- part2Ans 256 . parseSchool <$> readFile "input"
  putStr "Part 2: "
  print part2
