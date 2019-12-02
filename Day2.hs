import qualified Data.Map as Map

mkIntcode = Map.fromList . zip [0..]

opcode 1 = Just (+)
opcode 2 = Just (*)
opcode _ = Nothing

eval op cntr prgm = do
  x <- Map.lookup (cntr + 1) prgm
  y <- Map.lookup (cntr + 2) prgm
  z <- Map.lookup (cntr + 3) prgm
  w <- op <$> Map.lookup x prgm <*> Map.lookup y prgm
  apply (cntr + 4) (Map.insert z w prgm)

apply cntr prgm =
  case Map.lookup cntr prgm of
    Just 99 -> Just prgm
    Just n -> opcode n >>= \op -> eval op cntr prgm
    _ -> Nothing

seek target prgm =
  [ (noun, verb) | noun <- [0..99]
                 , verb <- [0..99]
                 , run noun verb prgm == Just target
                 ]
  where
    run noun verb =
      (=<<) (Map.lookup 0)
      . apply 0
      . Map.insert 2 verb
      . Map.insert 1 noun

main = do
  let
    decode raw = mkIntcode . read $ "[" <> raw <> "]"
    encode (noun, verb) = 100 * noun + verb

  input <- getContents
  print . encode . head . seek 19690720 . decode $ input