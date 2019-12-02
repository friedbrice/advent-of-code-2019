import Control.Monad (guard)
import qualified Data.Map as Map

mkIntcode = Map.fromList . zip [0..]

opcode 1 = Just (+)
opcode 2 = Just (*)
opcode _ = Nothing

eval op pointer program = do
  x <- Map.lookup (pointer + 1) program
  y <- Map.lookup (pointer + 2) program
  z <- Map.lookup (pointer + 3) program
  w <- op <$> Map.lookup x program <*> Map.lookup y program
  apply (pointer + 4) (Map.insert z w program)

apply pointer program =
  case Map.lookup pointer program of
    Just 99 -> Just program
    Just n -> opcode n >>= \op -> eval op pointer program
    _ -> Nothing

seek target program = do
  let
    run n v =
      (=<<) (Map.lookup 0)
      . apply 0
      . Map.insert 2 v
      . Map.insert 1 n

  noun <- [0..99]
  verb <- [0..99]
  guard (run noun verb program == Just target)
  return (noun, verb)

main = do
  let
    decode raw = mkIntcode . read $ "[" <> raw <> "]"
    encode (noun, verb) = 100 * noun + verb

  input <- getContents
  (print . encode . head . seek 19690720 . decode) input
