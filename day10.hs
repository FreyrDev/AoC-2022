import Data.Function ((&))
import Control.Monad ((<=<))
import Data.List.Split (chunksOf)
import Data.Functor ((<&>))

data Instruction = Noop | Add Int deriving Show

parse :: String -> [Instruction]
parse = (Noop:) . (parseInstrcts . words <=< lines)
  where parseInstrcts [_]   = [Noop]
        parseInstrcts [_,v] = [Noop,Add (read v)]

runInstrcts :: Int -> [Instruction] -> [Int]
runInstrcts x [] = []
runInstrcts x (Noop  : ins) = x   : runInstrcts x ins
runInstrcts x (Add v : ins) = x+v : runInstrcts (x+v) ins

sumSignals :: [Int] -> Int
sumSignals xs = sum $ map ((*) =<< (xs !!) . subtract 1) [20,60..220]

drawScreen :: [Int] -> String
drawScreen = splitLines . concat . zipWith drawPixel [0..]
  where drawPixel crt x = if abs (crt`mod`40 - x) <= 1 then "@@" else "  "
        splitLines = unlines . chunksOf 80

main :: IO ()
main = do
  input <- readFile "inputs/day10.txt" <&> parse <&> runInstrcts 1
  sumSignals input & print
  drawScreen input & putStr
