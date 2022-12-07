import Data.Function ((&))
import Data.List (find, isSuffixOf, sort)
import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import Control.Arrow ((&&&))


data Cmd = Dir | Ls | CdUp |Cd String | F String Int

data File = File String Int

type Path = [String]
type PathDict = [(Path,[File])]

fileSize :: File -> Int
fileSize (File _ size) = size

insertInto :: PathDict -> (Path,[File]) -> PathDict
insertInto xs x = x : xs

adjustDict :: ([File] -> [File]) -> Path -> PathDict -> PathDict
adjustDict f path dict = insertInto (filter (not . isPath) dict) . second f . fromJust . find isPath $ dict
  where isPath = (path ==) . fst

runCmd :: (Path, PathDict) -> Cmd -> (Path, PathDict)
runCmd (wd, dict)  Dir     = (wd, dict)
runCmd (wd, dict)  Ls      = (wd,      dict)
runCmd (wd, dict)  CdUp    = (tail wd, dict)
runCmd (wd, dict) (Cd dir) = (dir:wd,  insertInto dict (dir:wd, []))
runCmd (wd, dict) (F n sz) = (wd,      adjustDict (File n sz:) wd dict)

parse :: String -> PathDict
parse = populatePathDict . map (parseCmd . words) . lines
  where populatePathDict =  snd . foldl runCmd ([],[])
        parseCmd ["dir", _]        = Dir
        parseCmd ["$", "ls"]       = Ls
        parseCmd ["$", "cd", ".."] = CdUp
        parseCmd ["$", "cd", dir]  = Cd dir
        parseCmd [size, name]      = F name (read size)

recursiveSize :: PathDict -> [(Path, Int)]
recursiveSize = (map =<< flip (foldl sumIfInside)) . map (second (sum . map fileSize))
  where sumIfInside (px,vx) (py,vy) = if px `isSuffixOf` py && px /= py then (px,vx+vy) else (px,vx)

part1, part2 :: [(Path, Int)] -> Int
part1 = sum . filter (<= 100000) . map snd
part2 = fromJust . (find =<< (<=) . subtract 40000000 . last) . sort . map snd

main :: IO ()
main = do
  input <- readFile "inputs/day7.txt"
  parse input & recursiveSize & (part1 &&& part2) & print
