import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Foldable (traverse_)

data Result = WrongOrder | RightOrder | NoDecision deriving (Eq,Enum)
data Tree a = Leaf a | Branch [Tree a] deriving Read

instance Show a => Show (Tree a) where
  show :: Show a => Tree a -> String
  show (Branch xs) = show xs
  show (Leaf x) = show x

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace  _ _ [] = []
replace [] _ xs = xs
replace this that xs = 
  if this `isPrefixOf` xs
  then that   ++ replace this that (drop (length this) xs)
  else head xs : replace this that (tail xs)

parse :: String -> [([Tree Int],[Tree Int])]
parse = map ((\(Branch x, Branch y) -> (x, y)) . packetPair) . splitOn [""] . lines . treeString
  where treeString input = foldl leafString input [0..9] & replace "1Leaf " "1" & replace "[" "Branch [" 
        leafString acc x = replace (show x) ("Leaf " ++ show x) acc
        packetPair [x,y] = (read x, read y)

thenIfNecessary :: Result -> Result -> Result
thenIfNecessary x y = if x == NoDecision then y else x

checkOrder :: [Tree Int] -> [Tree Int] -> Result
checkOrder [] [] = NoDecision
checkOrder [] _  = RightOrder
checkOrder _  [] = WrongOrder
checkOrder (Branch x:xs) (Branch y:ys) = checkOrder x y        `thenIfNecessary` checkOrder xs ys
checkOrder (Branch x:xs)   (Leaf y:ys) = checkOrder x [Leaf y] `thenIfNecessary` checkOrder xs ys
checkOrder   (Leaf x:xs) (Branch y:ys) = checkOrder [Leaf x] y `thenIfNecessary` checkOrder xs ys
checkOrder   (Leaf x:xs)   (Leaf y:ys)
  | x == y    = checkOrder xs ys
  | otherwise = toEnum . fromEnum $ x < y

countIndicies :: [Result] -> Int
countIndicies = sum . zipWith (*) [1..] . map fromEnum

main :: IO ()
main = do
  input <- readFile "inputs/day13.txt"
  parse input & map (uncurry checkOrder) & countIndicies & print
