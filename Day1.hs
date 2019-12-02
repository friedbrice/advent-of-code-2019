fuelRequired =
  sum . takeWhile (> 0) . drop 1 . iterate rawFuel
  where
    rawFuel x = (x `div` 3) - 2

main = do
  input <- getContents
  print . sum . map (fuelRequired . read) . lines $ input
