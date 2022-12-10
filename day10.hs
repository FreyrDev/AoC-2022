import Data.Function ((&))
import Control.Monad ((<=<))
import Data.List.Split (chunksOf)
import Data.List (intercalate)
import Data.Functor ((<&>))

data Instruction = Noop | Add Int deriving Show

parse :: String -> [Instruction]
parse = (Noop:) . (parseInstrcts . words <=< lines)
  where parseInstrcts [_]   = [Noop]
        parseInstrcts [_,v] = [Noop,Add (read v)]

runInstrcts :: (Int, Int) -> [Instruction] -> [(Int, Int)]
runInstrcts (crt,x) [] = []
runInstrcts (crt,x) (Noop  : ins) = (crt,x)   : runInstrcts (crt+1,x) ins
runInstrcts (crt,x) (Add v : ins) = (crt,x+v) : runInstrcts (crt+1,x+v) ins

sumSignals :: [(Int,Int)] -> Int
sumSignals xs = sum $ map ((*) <*> snd . (xs !!) . subtract 1) [20,60..220]

drawScreen :: [(Int, Int)] -> [Char]
drawScreen = splitLines . (drawPixel <=< init)
  where drawPixel (crt,x) = if abs (crt`mod`40 - x) <= 1 then "@@" else "  "
        splitLines = intercalate "\n" .  chunksOf 80

main :: IO ()
main = do
  input <- readFile "inputs/day10.txt" <&> parse <&> runInstrcts (0,1)
  sumSignals input & print
  drawScreen input & putStrLn