import Data.List (elemIndex, foldl', sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (fromList, intersection, toList)

data Dir = Dx Int | Dy Int

parseDirs = map parseDir . splitOn ","

parseDir ('R':digits) = Dx (read digits)
parseDir ('U':digits) = Dy (read digits)
parseDir ('L':digits) = Dx (negate $ read digits)
parseDir ('D':digits) = Dy (negate $ read digits)

trace = reverse . foldl' step [(0, 0)]
  where
    step ((x, y):tail) (Dx n) = [(x', y) | x' <- span x n] <> tail
    step ((x, y):tail) (Dy n) = [(x, y') | y' <- span y n] <> tail
    span a n = reverse [a, a + signum n .. a + n]

main = do
  input <- getContents
  let
    [wire1, wire2] = map (trace . parseDirs) (lines input)
    crossings = toList (fromList wire1 `intersection` fromList wire2)
    dist p = fromJust (elemIndex p wire1) + fromJust (elemIndex p wire2)
  print . head . drop 1 . sort . map dist $ crossings
