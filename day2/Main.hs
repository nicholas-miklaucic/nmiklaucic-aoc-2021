import Debug.Trace
import System.IO

data Posn = Posn Int Int deriving (Show)

data Direction = Forward | Down | Up deriving (Show)

data Move = Move Direction Int deriving (Show)

parseDir :: String -> Direction
parseDir "forward" = Forward
parseDir "down" = Down
parseDir "up" = Up
parseDir s = error $ "Bad direction: " ++ s

parseLine :: String -> Move
parseLine = parseMove . words

parseMove :: [String] -> Move
parseMove [dir, amount] = Move (parseDir dir) (read amount)
parseMove w = error ("Invalid line: " ++ concat w)

applyMove :: Move -> Posn -> Posn
applyMove (Move Forward dx) (Posn x y) = Posn (x + dx) y
applyMove (Move Down dy) (Posn x y) = Posn x (y + dy)
applyMove (Move Up dy) (Posn x y) = Posn x (y - dy)

multXY :: Posn -> Int
multXY (Posn x y) = x * y

part1Ans :: [String] -> Int
part1Ans lines = multXY $ foldr (applyMove . parseLine) (Posn 0 0) $ reverse lines

data Posn3D = Posn3D Int Int Int

applyMove2 :: Move -> Posn3D -> Posn3D
applyMove2 (Move Forward dx) (Posn3D x y aim) = Posn3D (x + dx) (y + aim * dx) aim
applyMove2 (Move Down dy) (Posn3D x y aim) = Posn3D x y (aim + dy)
applyMove2 (Move Up dy) (Posn3D x y aim) = Posn3D x y (aim - dy)

multXY2 :: Posn3D -> Int
multXY2 (Posn3D x y aim) = x * y

part2Ans :: [String] -> Int
part2Ans lines = multXY2 $ foldr (applyMove2 . parseLine) (Posn3D 0 0 0) $ reverse lines

main :: IO ()
main = do
  part1 <- part1Ans . lines <$> readFile "input"
  putStr "Part 1: "
  print part1
  part2 <- part2Ans . lines <$> readFile "input"
  putStr "Part 2: "
  print part2
