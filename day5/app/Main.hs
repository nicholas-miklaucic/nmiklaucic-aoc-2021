import Data.List.Split
import Debug.Trace
import System.IO

data Posn = Posn Int Int deriving (Eq)

data Line = Line Posn Posn

parsePosn :: String -> Posn
parsePosn s = Posn x y
  where
    [x, y] = map read $ splitOn "," s

parseLine :: String -> Line
parseLine s = Line p1 p2
  where
    [p1, p2] = map parsePosn $ splitOn " -> " s

points :: Line -> [Posn]
points (Line (Posn x1 y1) (Posn x2 y2))
  | x1 == x2 = map (Posn x1) [ymin .. ymax]
  | y1 == y2 = map (`Posn` y1) [xmin .. xmax]
  | otherwise = []
  where
    xmin = min x1 x2
    xmax = max x1 x2
    ymin = min y1 y2
    ymax = max y1 y2

appearsTwice :: (Eq a) => [a] -> [a] -> [a]
appearsTwice [] acc = acc
appearsTwice (first : rest) acc
  | (first `notElem` acc) && (first `elem` rest) = appearsTwice rest (first : acc)
  | otherwise = appearsTwice rest acc

part1Ans :: [Line] -> Int
part1Ans l = length $ appearsTwice (concatMap points l) []

points2 :: Line -> [Posn]
points2 (Line (Posn x1 y1) (Posn x2 y2))
  | x1 == x2 = map (Posn x1) [ymin .. ymax]
  | y1 == y2 = map (`Posn` y1) [xmin .. xmax]
  | (x1 - x2) == (y1 - y2) = map (\n -> Posn (xmin + n) (ymin + n)) [0 .. xmax - xmin]
  | (x1 - x2) == (y2 - y1) = map (\n -> Posn (xmin + n) (ymax - n)) [0 .. xmax - xmin]
  | otherwise = []
  where
    xmin = min x1 x2
    xmax = max x1 x2
    ymin = min y1 y2
    ymax = max y1 y2

part2Ans :: [Line] -> Int
part2Ans l = length $ appearsTwice (concatMap points2 l) []

main :: IO ()
main = do
  part1 <- (part1Ans . map parseLine) . lines <$> readFile "input"
  putStr "Part 1: "
  print part1

  part2 <- (part2Ans . map parseLine) . lines <$> readFile "input"
  putStr "Part 2: "
  print part2
