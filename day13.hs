import Data.Function ((&), on)
import Data.List (isPrefixOf, sortBy, elemIndex)
import Data.List.Split (splitOn)
import GHC.Data.Maybe (fromJust)
import Control.Applicative (liftA2)


data Tree a = Leaf a | Node [Tree a] deriving (Eq,Read)

instance Show a => Show (Tree a) where
  show :: Show a => Tree a -> String
  show (Node xs) = show xs
  show (Leaf x)  = show x

infix 4 >->
(>->) :: Eq a => [a] -> [a] -> [a] -> [a]
(_  >-> _) [] = []
([] >-> _) xs = xs
(this >-> that) xs = 
  if this `isPrefixOf` xs
  then that   ++ (this >-> that) (drop (length this) xs)
  else head xs : (this >-> that) (tail xs)

parse :: String -> [([Tree Int],[Tree Int])]
parse = map ((\(Node x, Node y) -> (x, y)) . packetPair) . splitOn [""] . lines . treeStr
  where treeStr input = foldl leafStr input [0..9] & ("1Leaf " >-> "1") & ("[" >-> "Node [")
        leafStr acc x = (show x >-> "Leaf " ++ show x) acc
        packetPair [x,y] = (read x, read y)

addDividers :: [([Tree Int],[Tree Int])] -> [[Tree Int]]
addDividers = ([Node [Leaf 6]]:) . ([Node [Leaf 2]]:) . concatMap (\(x,y) -> [x,y])

thenIfNecessary :: Ordering -> Ordering -> Ordering
thenIfNecessary x y = if x == EQ then y else x

checkOrder :: [Tree Int] -> [Tree Int] -> Ordering
checkOrder [] [] = EQ
checkOrder [] _  = LT
checkOrder _  [] = GT
checkOrder (Node x:xs) (Node y:ys) = checkOrder x y        `thenIfNecessary` checkOrder xs ys
checkOrder (Node x:xs) (Leaf y:ys) = checkOrder x [Leaf y] `thenIfNecessary` checkOrder xs ys
checkOrder (Leaf x:xs) (Node y:ys) = checkOrder [Leaf x] y `thenIfNecessary` checkOrder xs ys
checkOrder (Leaf x:xs) (Leaf y:ys)
  | x == y    = checkOrder xs ys
  | otherwise = compare x y

countRightOrder :: [Ordering] -> Int
countRightOrder = sum . zipWith (*) [1..] . map (abs . (`div` 2) . subtract 2 . fromEnum)

findDecoder :: [[Tree Int]] -> Int
findDecoder = ((*) `on` succ.fromJust) <$> elemIndex [Node [Leaf 2]] <*> elemIndex [Node [Leaf 6]]

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day13.txt"
  map (uncurry checkOrder) input & countRightOrder & print
  addDividers input & sortBy checkOrder & findDecoder & print
