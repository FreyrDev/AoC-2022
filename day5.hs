import Data.Function ((&))
import Data.Char (isAlpha)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***))


(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(.:)  ::     Functor f =>   (b -> c) -> f (a -> b) -> f (a -> c)
(<.>) = liftA2 (.)  -- (g <.> f) x y = g x (f x y)
(.:)  = fmap . (.)  -- (g .: f)  x y = g   (f x y)

data Move = Move Int Int Int
numToMove :: Move -> Int
numToMove (Move x _ _) = x

parse :: String -> ([String], [Move])
parse = (part1 *** part2) . (\[x,y] -> (x,y)) . splitOn [""] . lines
  where part1 = filter (not . null) . map (filter isAlpha) . transpose
        part2 = map ((\[_,x,_,y,_,z] -> Move x (y-1) (z-1)) . map read . words)

push, pop :: Int -> Move -> [[a]] -> [[a]]
push n (Move _ fr to) stacks =
  let (xs,y:ys) = splitAt to stacks in xs ++ (take n (stacks !! fr) ++ y) : ys
pop  n (Move _ fr to) stacks =
  let (xs,y:ys) = splitAt fr stacks in xs ++ drop n y : ys

move1 :: Move -> [String] -> [String]
move1 = loop =<< numToMove
  where loop 0 = const id
        loop i = loop (i-1) <.> pop 1 <.> push 1

move2 :: Move -> [String] -> [String]
move2 = (pop =<< numToMove) <.> (push =<< numToMove)

doMoves :: ([String], [Move]) -> ([Char], [Char])
doMoves = uncurry doMoves1 &&& uncurry doMoves2
  where doMoves1 = map head .: foldl (flip move1)
        doMoves2 = map head .: foldl (flip move2)

main :: IO ()
main = do
  input <- readFile "inputs/day5.txt"
  parse input & doMoves & print
