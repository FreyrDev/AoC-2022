import Data.Function ((&))
import Data.Set (toList, fromList)
import Control.Arrow ((&&&))


uniqueSeqOf :: Int -> [Char] -> Int
uniqueSeqOf n = loop n 0 . splitAt n
  where loop i uniq (xs,y:ys) 
          | uniq == n = i-1
          | otherwise = loop (i+1)  (uniqueElems xs) (tail xs ++ [y], ys)
        uniqueElems = length . toList . fromList

main :: IO ()
main = print . (uniqueSeqOf 4 &&& uniqueSeqOf 14) =<< readFile "inputs/day6.txt"