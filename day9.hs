import Data.Function ((&))
import Data.Foldable (traverse_)
import Control.Monad ((<=<))
import Data.Set (fromList, toList)


data Move = U | D | R | L deriving (Show,Read)

type Coord = (Int,Int)
type Rope = [Coord]

x, y :: Coord -> Int
x = fst; y = snd

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

parse :: String -> [Move]
parse = read .: splitMoves <=< lines
  where splitMoves (x:_:n) = replicate (read n) [x]

moveHead :: Rope -> Move -> Rope
moveHead (h:ts) U = (x h, y h + 1) : ts
moveHead (h:ts) D = (x h, y h - 1) : ts
moveHead (h:ts) R = (x h + 1, y h) : ts
moveHead (h:ts) L = (x h - 1, y h) : ts

moveTail :: Rope -> Rope
moveTail [t] = [t]
moveTail (h:t:ts)
  | adjacent  = h : moveTail (t:ts)
  | otherwise = h : moveTail (newTail:ts)
  where adjacent  = abs (diff x) <= 1 && abs (diff y) <= 1
        newTail   = (move x, move y)
        move axis = axis t + signum (diff axis)
        diff axis = axis h - axis t

moveRope :: Int -> [Move] -> [Rope]
moveRope n = scanl (moveTail .: moveHead) (replicate n (0,0))

tailPositions :: [Rope] -> [Coord]
tailPositions = toList . fromList . map last

main :: IO ()
main = do
  input <- readFile "inputs/day9.txt"
  parse input & moveRope 2  & tailPositions & length & print
  parse input & moveRope 10 & tailPositions & length & print
