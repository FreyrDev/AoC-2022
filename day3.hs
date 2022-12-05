import Data.Char (ord, isLower)
import Data.Function ((&))
import Control.Arrow ((&&&))

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

group3 :: [a] -> [(a,a,a)]
group3 = group3' []
  where group3' acc [] = acc
        group3' acc xs = group3' ((take 3 xs & tupleList3) : acc) (drop 3 xs)
        tupleList3 = \[x,y,z] -> (x,y,z)

intersect  :: Eq a => ([a],[a])     -> [a]
intersect3 :: Eq a => ([a],[a],[a]) -> [a]
intersect  (x,y)   = filter (`elem` x) y
intersect3 (x,y,z) = intersect (intersect (x,y), z)

priority :: Char -> Int
priority x | isLower x = ord x - 96
           | otherwise = ord x - 38

scores :: [String] -> (Int, Int)
scores = score1 &&& score2
  where score1 = score . map (intersect . halve)
        score2 = score . map intersect3 . group3
        score  = sum . map (priority . head)

main :: IO ()
main = (print . scores . lines) =<< readFile "inputs/day3.txt"
