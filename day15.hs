import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Char (isNumber)
import Data.List (nub)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as S (map)
import Data.Set (Set,fromList,unions,(\\))
import Control.Arrow ((***))

type Coord = (Int,Int)
type SensorData = (Coord,Int)

manhattanDist :: Num a => (a, a) -> (a, a) -> a
manhattanDist (x1,y1) (x2,y2) =  abs (x1-x2) + abs (y1-y2)

parse :: String -> (Set SensorData, Set Coord)
parse = (fromList *** fromList) . unzip . map (parseSensors .  map read . filterToNumbers . words) . lines
  where filterToNumbers = filter (not . null) . map (filter ((||) <$> isNumber <*> (=='-')))
        parseSensors coords@[x1,y1,x2,y2] = (((x1,y1), manhattanDist (x1,y1) (x2,y2)),(x2,y2))

withinDistOnRow :: Int -> SensorData -> Set Coord
withinDistOnRow n ((x1,y1),d) = fromList [(x1+x2,n) | x2 <- [-d..d], manhattanDist (0,0) (x2,n-y1) <= d]

main :: IO ()
main = do
  (sensorData,beacons) <- readFile "inputs/day15.txt" <&> parse
  S.map (withinDistOnRow 2000000) sensorData & unions & (\\ beacons) & length & print
