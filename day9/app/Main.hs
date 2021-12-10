import Data.List
import Data.MultiSet (MultiSet, distinctElems, fromList, occur)
import qualified Data.MultiSet (map)
import Data.Ord
import Debug.Trace
import System.IO

data Posn = Posn Int Int deriving (Eq, Ord)

data Heatmap = Heatmap Int Int [[Int]] deriving (Show)

allPoints :: Heatmap -> [Posn]
allPoints (Heatmap h w vals) = concat [[Posn x y | x <- [0 .. w -1]] | y <- [0 .. h -1]]

getPoint :: Heatmap -> Posn -> Int
getPoint (Heatmap h w vals) (Posn row col) = vals !! col !! row

isInBounds :: Heatmap -> Posn -> Bool
isInBounds (Heatmap h w vals) (Posn x y) = x `elem` [0 .. w -1] && y `elem` [0 .. h -1]

neighbors :: Heatmap -> Posn -> [Posn]
neighbors hmap@(Heatmap h w vals) (Posn x y) = filter (isInBounds hmap) allNeighbors
  where
    allNeighbors = [Posn (x + 1) y, Posn (x - 1) y, Posn x (y + 1), Posn x (y - 1)]

isLowPoint :: Heatmap -> Posn -> Bool
isLowPoint hmap pos = all ((getPoint hmap pos <) . getPoint hmap) (neighbors hmap pos)

riskLevel :: Heatmap -> Posn -> Int
riskLevel hmap pos = if isLowPoint hmap pos then 1 + getPoint hmap pos else 0

part1Ans :: Heatmap -> Int
part1Ans hmap = sum $ map (riskLevel hmap) $ allPoints hmap

readHeatmap :: String -> Heatmap
readHeatmap str = Heatmap h w vals
  where
    vals = map (map (\c -> read [c])) $ lines str
    h = length $ lines str
    w = length $ head $ lines str

nextLowest :: Heatmap -> Posn -> Posn
nextLowest hmap pos = minimumBy (comparing (getPoint hmap)) (neighbors hmap pos ++ [pos])

fall :: Heatmap -> MultiSet Posn -> MultiSet Posn
fall hmap = Data.MultiSet.map (nextLowest hmap)

basins :: Heatmap -> MultiSet Posn
basins hmap = f pts (fall hmap pts) hmap
  where
    pts = fromList $ filter (\x -> getPoint hmap x /= 9) $ allPoints hmap
    f prev curr hmap
      | prev == curr = curr
      | otherwise = f curr (fall hmap curr) hmap

part2Ans :: Heatmap -> Int
part2Ans hmap = product (take 3 sortedSizes)
  where
    basinSet = basins hmap
    sizes = map (`occur` basinSet) (distinctElems basinSet)
    sortedSizes = reverse $ sort sizes

main :: IO ()
main = do
  hmap <- readHeatmap <$> readFile "input"
  putStr "Part 1: "
  print $ part1Ans hmap

  putStr "Part 2: "
  print $ part2Ans hmap
