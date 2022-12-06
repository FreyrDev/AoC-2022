import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

uniqueSeqOf :: Ord a => Int -> [a] -> Int
uniqueSeqOf n xs = (+n) . fromJust . elemIndex n . map (length . nub . take n . (`drop` xs)) $ [0..]

main :: IO ()
main = print . (uniqueSeqOf 4 &&& uniqueSeqOf 14) =<< readFile "inputs/day6.txt"