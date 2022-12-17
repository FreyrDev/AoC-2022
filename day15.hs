import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Char (isNumber)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as S (map)
import Data.Set (Set,fromList,unions,(\\), intersection, toList)
import Control.Arrow ((***))
import Data.List (sort, group, maximumBy, transpose)
import Data.Ord (comparing)

type Coord = (Int,Int)
type SensorData = (Coord,Int)

manhattanDist :: Num a => (a, a) -> (a, a) -> a
manhattanDist (x1,y1) (x2,y2) =  abs (x1-x2) + abs (y1-y2)

parse :: String -> ([SensorData], Set Coord)
parse = fmap fromList . unzip . map (parseSensors .  map read . filterToNumbers . words) . lines
  where filterToNumbers = filter (not . null) . map (filter ((||) <$> isNumber <*> (=='-')))
        parseSensors coords@[x1,y1,x2,y2] = (((x1,y1), manhattanDist (x1,y1) (x2,y2)),(x2,y2))

withinDistOnRow :: Int -> SensorData -> Set Coord
withinDistOnRow n ((x1,y1),d) = fromList [(x1+x2,n) | x2 <- [-d..d], manhattanDist (0,0) (x2,n-y1) <= d]

borders :: SensorData -> [(Coord,Coord)]
-- atBorder ((x1,y1),d) = nubOrd [(x1+x2,y1+y2) | x2 <- [-(d+1)..(d+1)], y2 <- [(d+1) - abs x2, negate ((d+1) - abs x2)]]
-- atBorder ((x,y),d)   = [[1,y+d+1-x,x-d-1,x], [-1,y+d+1+x,x,x+d+1], [1,y-d-1-x,x,x+d+1], [-1,y-d-1+x,x-d-1,x]]
borders ((x,y),d') = let d = d' + 1 in [((x-d,y),(x,y+d)), ((x,y+d),(x+d,y)), ((x+d,y),(x,y-d)), ((x,y-d),(x-d,y))]

-- findIntersections :: [(Coord,Coord)] -> [(Coord,Coord)] -> [Coord]
-- findIntersections = foldl (\acc () -> )

mostCommon :: [Coord] -> Coord
mostCommon = head . maximumBy (comparing length) . group . sort

main :: IO ()
main = do
  (sensorData,beacons) <- readFile "inputs/day15.txt" <&> parse
  map (withinDistOnRow 2000000) sensorData & unions & (\\ beacons) & length & print
  -- map borders (toList sensorData) & transpose & print

-- ..^..
-- ./.\.
-- ‹...›
-- .\./.
-- ..v..
