import Data.Function ((&))
import Data.List (find, isSuffixOf, sort)
import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import Control.Arrow ((&&&))
import GHC.Utils.Misc (filterOut)


data Cmd = Dir | Ls | CdUp |Cd String | F Int

newtype File = File {fileSize :: Int}

type Path = [String]
type PathDict = [(Path,[File])]

insertInto :: PathDict -> (Path,[File]) -> PathDict
insertInto xs x = x : xs

insertAtPath :: Path -> ([File] -> [File]) -> PathDict -> PathDict
insertAtPath path f = (insertInto . filterOut isPath) <*> (second f . fromJust . find isPath)
  where isPath = (path ==) . fst

runCmd :: (Path, PathDict) -> Cmd -> (Path, PathDict)
runCmd (wd, dict)  Dir     = (wd,      dict)
runCmd (wd, dict)  Ls      = (wd,      dict)
runCmd (wd, dict)  CdUp    = (tail wd, dict)
runCmd (wd, dict) (Cd dir) = (dir:wd,  insertInto dict (dir:wd, []))
runCmd (wd, dict) (F size) = (wd,      insertAtPath wd (File size:) dict)

parse :: String -> PathDict
parse = populatePathDict . map (parseCmd . words) . lines
  where populatePathDict =  snd . foldl runCmd ([],[])
        parseCmd ["dir", _       ] = Dir
        parseCmd ["$", "ls"      ] = Ls
        parseCmd ["$", "cd", ".."] = CdUp
        parseCmd ["$", "cd", dir ] = Cd dir
        parseCmd [size, _        ] = F (read size)

recursiveSize :: PathDict -> [(Path, Int)]
recursiveSize = (map =<< flip (foldl sumIfInside)) . map (second $ sum . map fileSize)
  where sumIfInside (px,vx) (py,vy) = if px `isProperSuffixOf` py then (px,vx+vy) else (px,vx)
        isProperSuffixOf x y = x `isSuffixOf` y && x /= y

part1, part2 :: [(Path, Int)] -> Int
part1 = sum . filter (<= 100000) . map snd
part2 = fromJust . (find =<< (<=) . subtract 40000000 . last) . sort . map snd

main :: IO ()
main = do
  input <- readFile "inputs/day7.txt"
  parse input & recursiveSize & (part1 &&& part2) & print
