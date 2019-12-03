import Control.Monad (guard)
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

apply cntr prgm = do
  n <- Map.lookup cntr prgm
  if n == 99 then return prgm
  else do
    op <- opcode n
    eval op cntr prgm

seek target prgm = do
  let
    run noun verb =
      (=<<) (Map.lookup 0)
      . apply 0
      . Map.insert 2 verb
      . Map.insert 1 noun

  noun <- [0..99]
  verb <- [0..99]
  guard (run noun verb prgm == Just target)
  return (noun, verb)

main = do
  let
    decode raw = mkIntcode . read $ "[" <> raw <> "]"
    encode (noun, verb) = 100 * noun + verb

  input <- getContents
  print . encode . head . seek 19690720 . decode $ input
