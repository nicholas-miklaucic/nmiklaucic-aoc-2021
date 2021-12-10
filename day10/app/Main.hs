import Data.Either
import Data.List
import Data.Maybe
import Data.Stack

data DelimType = Paren | Square | Curly | Angle deriving (Eq)

data DelimKind = Start | End

type Delim = (DelimType, DelimKind)

parseDelim :: Char -> Delim
parseDelim '(' = (Paren, Start)
parseDelim ')' = (Paren, End)
parseDelim '[' = (Square, Start)
parseDelim ']' = (Square, End)
parseDelim '{' = (Curly, Start)
parseDelim '}' = (Curly, End)
parseDelim '<' = (Angle, Start)
parseDelim '>' = (Angle, End)
parseDelim c = error "Invalid character"

makeStack :: [Delim] -> Either DelimType (Stack DelimType)
makeStack = f stackNew
  where
    f :: Stack DelimType -> [Delim] -> Either DelimType (Stack DelimType)
    f stack [] = Right stack
    f stack ((dtype, Start) : rest) = f (stackPush stack dtype) rest
    f stack ((dtype, End) : rest) =
      if beginType == Just dtype
        then f (fromJust restStack) rest
        else Left dtype
      where
        popped = stackPop stack
        restStack = fst <$> popped
        beginType = snd <$> popped

errScore :: Either DelimType (Stack DelimType) -> Int
errScore (Left Paren) = 3
errScore (Left Square) = 57
errScore (Left Curly) = 1197
errScore (Left Angle) = 25137
errScore _ = 0

part1Ans :: [[Delim]] -> Int
part1Ans rows = sum $ map (errScore . makeStack) rows

complete :: Stack DelimType -> [DelimType]
complete stack = case stackPop stack of
  Just (curr, rest) -> rest : complete curr
  Nothing -> []

autoScore :: [DelimType] -> Int
autoScore = foldl score 0
  where
    score :: Int -> DelimType -> Int
    score i dtype =
      5 * i + case dtype of
        Paren -> 1
        Square -> 2
        Curly -> 3
        Angle -> 4

median :: [Int] -> Int
median ints = (sort ints) !! (length ints `div` 2)

part2Ans :: [[Delim]] -> Int
part2Ans delims = median $ map (autoScore . complete . fromRight stackNew) $ filter isRight (map makeStack delims)

main :: IO ()
main = do
  rows <- map (map parseDelim) <$> (lines <$> readFile "input")
  putStr "Part 1: "
  print $ part1Ans rows

  putStr "Part 2: "
  print $ part2Ans rows
