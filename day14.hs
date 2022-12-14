import Data.Function ((&))
import Control.Applicative (liftA2)
import Data.Set as Set (Set, fromList, map, delete, insert, notMember, size)
import Data.Functor ((<&>))

type Coord = (Int,Int)
type Cave = Set Coord

parse :: String -> Cave
parse = fromList . concatMap (makeLines . endPoints) . lines
  where endPoints = Prelude.map (read . ("(" ++) . (++ ")")) . filter (/="->") . words
        makeLines [_] = []
        makeLines ((x1,y1):(x2,y2):xys) = liftA2 (,) (x1...x2) (y1...y2) ++ makeLines ((x2,y2):xys)
        x...y = [min x y..max x y] 

stepSand :: Int -> Coord -> Cave -> Cave
stepSand low (x,y) cave
  | y == low = stepSand low (500,0) cave
  | (x  ,y+1) `notMember` cave = stepSand low (x  ,y+1) (replace (x,y) (x  ,y+1) cave)
  | (x-1,y+1) `notMember` cave = stepSand low (x-1,y+1) (replace (x,y) (x-1,y+1) cave)
  | (x+1,y+1) `notMember` cave = stepSand low (x+1,y+1) (replace (x,y) (x+1,y+1) cave)
  | (x,y) == (500,0) = insert (500,0) cave
  | otherwise = stepSand low (500,0) cave
  where replace this that = delete this <&> insert that

main :: IO ()
main = do
  caveRocks <- readFile "inputs/day14.txt" <&> parse
  let lowPoint = caveRocks & Set.map snd & maximum & succ
  caveRocks & stepSand lowPoint (500,0) & size & subtract (size caveRocks) & print