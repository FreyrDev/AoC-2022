import Data.Function ((&))
import Control.Arrow ((&&&),(***))
import Control.Monad (join)
import Data.Char (ord)

parse :: String -> [(Int,Int)]
parse = map ((\[x,y] -> (x,y)) . map rps . join . words) . lines
  where rps x = (ord x - 65) `mod` 23

scores :: [(Int, Int)] -> (Int, Int)
scores = mapTuple sum . unzip . map (mapTuple (uncurry (+)) . subscores)
  where mapTuple  = join (***)
        subscores = (shape1 &&& result1) &&& (shape2 &&& result2)
        shape1  (_,y) = y + 1
        result1 (x,y) = (y - x + 1) `mod` 3 * 3
        shape2  (x,y) = (x + y - 1) `mod` 3 + 1
        result2 (_,y) = y * 3

main :: IO ()
main = do
  input <- readFile "inputs/day2.txt"
  print (input & parse & scores)



-- shape2  (x,y) = (x - (2 - (y - 2) `mod` 3)) `mod` 3 + 1


-- cycleRPS :: Int -> Int
-- cycleRPS 1 = 3
-- cycleRPS x = pred x

-- parse :: String -> [[Int]]
-- parse = map (map toRPS . concat . words) . lines
--   where toRPS x
--           | x `elem` "AX" = 1
--           | x `elem` "BY" = 2
--           | otherwise     = 3

-- score :: [[Int]] -> Int
-- score = sum . map (liftA2 (+) last outcome)
--   where outcome x
--           | head x == last x = 3
--           | head x == cycleRPS (last x) = 6
--           | otherwise = 0

-- score' :: [[Int]] -> Int
-- score' = sum . map (liftA2 (+) shape outcome)
--   where shape x = case last x of
--           1 -> cycleRPS . head $ x
--           2 -> head x
--           _ -> cycleRPS . cycleRPS . head $ x
--         outcome x = 3 * last x - 3