import Data.List (group)
import Data.List.Split (splitOn)

main = do
  input <- getContents
  let [_, lo, hi] = 0 : (map read . splitOn "-" $ input)
  print $ length (possibilities lo hi)

possibilities lo hi = do
  filter (\n -> lo <= n && n <= hi)
  . map read
  . filter stuttering
  . filter increasing
  . sequence
  $ replicate 6 ['0'..'9']

increasing (_:[]) = True
increasing (x1:x2:rest) = x1 <= x2 && increasing (x2:rest)

stuttering = not . null . filter ((2 ==) . length) . group
