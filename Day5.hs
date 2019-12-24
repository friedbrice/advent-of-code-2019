import Debug.Trace (trace)

import Control.Monad (guard, join)
import Data.Foldable (toList)
import qualified Data.Map as Map

debug msg val x = trace (show (msg, val)) x

debugMaybe msg val x = case x of
  Just y -> Just y
  Nothing -> debug msg val Nothing

type MemMap = Map.Map Int Int

data Runtime = Runtime
  { counter :: Int
  , memory :: MemMap
  , input :: [Int]
  , output :: [Int]
  } deriving Show

load :: [Int] -> [Int] -> Runtime
load inp pgrm = Runtime 0 (Map.fromList . zip [0..] $ pgrm) inp []

data Mode = Ref | Lit deriving Show

modecode :: Int -> Maybe Mode
modecode 0 = Just Ref
modecode 1 = Just Lit
modecode n = debug "modecode" n Nothing

readMem :: Mode -> Int -> MemMap -> Maybe Int
readMem Lit c m = Map.lookup c m
readMem Ref c m = Map.lookup c m >>= \c' -> Map.lookup c' m

reduce :: Int -> Runtime -> Maybe Runtime
reduce n rt =
  case
    ( modecode . (`mod` 10) . (`div` 1000) $ n
    , modecode . (`mod` 10) . (`div` 100) $ n
    , n `mod` 100
    )
  of
    (m2, m1, 1) -> join (binop (+) <$> m1 <*> m2 <*> pure rt)
    (m2, m1, 2) -> join (binop (*) <$> m1 <*> m2 <*> pure rt)
    (_, _, 3) -> readInput rt
    (_, _, 4) -> writeOutput rt
    _ -> debug "reduce" (n, rt) Nothing

binop :: (Int -> Int -> Int) -> Mode -> Mode -> Runtime -> Maybe Runtime
binop f m1 m2 rt@Runtime{ counter = cnt, memory = mem } = do
  x1 <- debugMaybe "binop x1" (m1, m2, rt) $ readMem m1 (cnt + 1) mem
  x2 <- debugMaybe "binop x2" (m1, m2, rt) $ readMem m2 (cnt + 2) mem
  addr <- debugMaybe "binop addr" (m1, m2, rt) $ readMem Lit (cnt + 3) mem
  return rt{ counter = cnt + 4, memory = Map.insert addr (f x1 x2) mem }

readInput :: Runtime -> Maybe Runtime
readInput rt@Runtime{ counter = cnt, memory = mem, input = x:xs } = do
  ref <- debugMaybe "readInput ref" rt $ readMem Lit (cnt + 1) mem
  return rt{ counter = cnt + 2, memory = Map.insert ref x mem, input = xs }

writeOutput :: Runtime -> Maybe Runtime
writeOutput rt@Runtime{ counter = cnt, memory = mem, output = xs } = do
  x <- debugMaybe "writeOutput x" rt $ readMem Ref (cnt + 1) mem
  trace (show (cnt, x)) $ return rt{ counter = cnt + 2, output = x:xs }

eval :: Runtime -> Maybe Runtime
eval rt = do
  n <- debugMaybe "eval n" rt $ Map.lookup (counter rt) (memory rt)
  if n == 99
    then return rt
    else eval =<< reduce n rt

main :: IO ()
main = do
  input <- getContents
  print . eval . load [1] . read $ "[" <> input <> "]"
