import Prelude  hiding (lookup)
import Data.List       (elemIndex,findIndex,transpose)
import Data.Char       (ord)
import Data.Function   ((&))
import Data.Functor    ((<&>),($>))
import Data.Maybe      (fromJust)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map,fromList,lookup,mapWithKey)


type Tiles = Map (Int,Int) (Char,Int)

indexPosition :: Int -> Int -> (Int,Int)
indexPosition width i = (i `mod` width,i `div` width)

parse :: Int -> [String] -> Tiles
parse width = fromList . zipWith toTile [0..] . concat
  where toTile i elev
          | elev == 'S' || elev == 'a' = (indexPosition width i,('a',0))
          | elev == 'E' = (indexPosition width i,('z',maxBound))
          | otherwise   = (indexPosition width i,(elev,maxBound))

endPosition :: Int -> [String] -> (Int,Int)
endPosition width = indexPosition width . fromJust . elemIndex 'E' . concat

update :: Maybe (Char,Int) -> (Char,Int) -> (Char,Int)
update Nothing tile = tile
update (Just (fromElev,fromSteps)) tile@(elev,steps) =
  if canReach && steps > fromSteps
    then tile $> fromSteps + 1
    else tile
  where canReach = ord elev - ord fromElev <= 1

progress :: Tiles -> Tiles
progress tiles = mapWithKey (\(x,y) tile -> 
  update (lookup (x,y-1) tiles) . 
  update (lookup (x-1,y) tiles) . 
  update (lookup (x,y+1) tiles) . 
  update (lookup (x+1,y) tiles) $ tile) tiles

reached :: (Int,Int) -> Tiles -> Bool
reached posEnd = (< maxBound) . snd . fromJust . lookup posEnd

showTiles :: Int -> Tiles -> String 
showTiles height = unlines . transpose . chunksOf height . foldl (\xs x -> xs ++ [showTile x]) []
  where showTile (_,steps)
          | steps == maxBound = ' '
          | otherwise = ".:;+=xX&$%" !! ((10*steps) `div` 541)

main :: IO ()
main = do
  input <- readFile "inputs/day12.txt" <&> lines
  let width  = head input & length
  let height = length input
  let tiles = parse width input
  let end   = endPosition width input
  let paths = iterate progress tiles
  paths !! 540 & showTiles height & putStrLn
  iterate progress tiles & findIndex (reached end) & fromJust & print
