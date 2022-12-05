import Text.Read (readMaybe)
import Data.List (sortBy)
import Data.Function ((&), fix)
import Control.Arrow ((&&&))
import Unsafe.Coerce (unsafeCoerce)

parse :: String -> [Maybe Int]
parse = map readMaybe . lines

top3 :: [Maybe Int] -> [Int]
top3 = take 3 . sortBy (flip compare) . map sum . split [] []
  where split out acc [] = out
        split out acc (x:xs) =
          case x of Just y  -> split out (y:acc) xs
                    Nothing -> split (acc:out) [] xs

main :: IO ()
main = do
  input <- readFile "inputs/day1.txt"
  parse input & top3 & head & print
  parse input & top3 & sum & print

-- -- main = readFile "inputs/day1.txt" >>= (print . (head &&& sum) . take 3 . sortBy (flip compare) . map sum . fix (\split out acc xs -> if null xs then out else case head xs of {Just y  -> split out (y:acc) (tail xs); Nothing -> split (acc:out) [] (tail xs)}) [] [] . map readMaybe . lines)

-- -- main = print $ (\f -> (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))) (\fac n -> if n == 0 then 1 else n * fac (n - 1)) 5

-- import Data.List (sort)
-- import Data.List.Split ( splitOn )

-- your1 input = maximum $ map (sum . map (\x -> read x :: Int)) $ splitOn [""] $ lines input
-- your2 input = sum $ take 3 $ reverse $ sort $ map (sum . map (\x -> read x :: Int)) $ splitOn [""] $ lines input

-- part1 = maximum . map (sum . map read) . splitOn [""] . lines
-- part2 = sum . take 3 . reverse . sort . map (sum . map read) . splitOn [""] . lines

-- main :: IO ()
-- main = do
--   input <- readFile "inputs/day1.txt"
--   part1 input & print
--   part2 input & print