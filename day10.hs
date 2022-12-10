import Data.Function ((&))
import Control.Monad ((<=<))

data Instruction = Noop | Add Int deriving Show

parse :: String -> [Instruction]
parse = (Noop:) . (toIns . words <=< lines)
  where toIns [_]   = [Noop]
        toIns [_,v] = [Noop,Add (read v)]

doInstructions :: (Int, Int) -> [Instruction] -> [(Int, Int)]
doInstructions (crt,x) [] = []
doInstructions (crt,x) (Noop  : ins) = (crt,x)   : doInstructions (crt+1,x) ins
doInstructions (crt,x) (Add v : ins) = (crt,x+v) : doInstructions (crt+1,x+v) ins

sumSignals :: [(Int,Int)] -> Int
sumSignals xs = sum $ map ((*) <*> snd . (xs !!) . subtract 1) [20,60..220]

drawScreen :: [(Int, Int)] -> [Char]
drawScreen = splitLines . (drawPixel <=< init)
  where drawPixel (crt,x) = if abs (crt`mod`40 - x) <= 1 then "##" else "  "
        splitLines [] = []
        splitLines xs = take 80 xs ++ '\n' : splitLines (drop 80 xs)

main :: IO ()
main = do
  input <- readFile "inputs/day10.txt"
  parse input & doInstructions (0,1) & sumSignals & print
  parse input & doInstructions (0,1) & drawScreen & putStr