import Data.List (elemIndex, nub, tails)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

uniqueSeqOf :: Ord a => Int -> [a] -> Int
uniqueSeqOf n = (+n) . fromJust . elemIndex n . map (length . nub . take n) . tails

main :: IO ()
main = print . (uniqueSeqOf 4 &&& uniqueSeqOf 14) =<< readFile "inputs/day6.txt"