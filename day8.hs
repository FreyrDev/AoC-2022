import Data.Function ((&), on)
import Data.List (transpose)
import Control.Applicative (liftA2)
import Control.Arrow ((***), second)
import Control.Monad (join)


infixr 8 .:
(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

zipMatrixWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrixWith = zipWith . zipWith

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x xs -> x : if p x then [] else xs) []

parse :: String -> ([String], [String])
parse = liftA2 (,) id transpose . lines

checkWith :: (Char -> String -> String -> Int) -> Int -> [String] -> [[Int]]
checkWith f id = map (\(x:y:xs) -> loop [id] [x] y xs)
  where loop acc _ _ [] = reverse (id:acc)
        loop acc l x r  = loop (f x l r : acc) (x : l) (head r) (tail r)

isVisible, scenicVal :: Char -> String -> String -> Int
isVisible x = fromEnum .: (||) `on` all (< x)
scenicVal x = (*) `on` length . takeUntil (>= x)

combineWith :: (a -> b -> c) -> ([c] -> c) -> ([[a]], [[b]]) -> c
combineWith f g = (fmap <*> map) g . uncurry (zipMatrixWith f) . second transpose

intOr :: Int -> Int -> Int
intOr = fromEnum . (/= 0) .: (+)

main :: IO ()
main = do
  input <- readFile "inputs/day8.txt"
  parse input & mapTuple (checkWith isVisible 1) & combineWith intOr sum & print
  parse input & mapTuple (checkWith scenicVal 0) & combineWith (*) maximum & print
