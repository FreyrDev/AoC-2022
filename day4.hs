import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.Bits ((.|.), (.&.), bit)
import Control.Applicative (liftA2)


(>->) :: String -> String -> String -> String
(_ >-> _) [] = []
(this >-> that) input =
  if this `isPrefixOf` input
  then that ++ (this >-> that) (drop (length this) input)
  else head input : (this >-> that) (tail input)

parse :: String -> [((Int,Int),(Int,Int))]
parse = map (read . ("((" ++) . (++ "))") . ("-" >-> ",") . ("," >-> "),(")) . lines

bitRange :: Int -> Int -> Integer
bitRange x y = sum $ map bit [x..y]

contains :: ((Int,Int),(Int,Int)) -> Int
contains ((a,b),(c,d)) = fromEnum $ liftA2 (||) (ab ==) (cd ==) (ab .|. cd)
  where ab = bitRange a b
        cd = bitRange c d

overlaps :: ((Int,Int),(Int,Int)) -> Int
overlaps ((a,b),(c,d)) = fromEnum . (/= 0) $ bitRange a b .&. bitRange c d

main :: IO ()
main = do
  input <- readFile "inputs/day4.txt"
  print (input & parse & map contains & sum)
  print (input & parse & map overlaps & sum)
