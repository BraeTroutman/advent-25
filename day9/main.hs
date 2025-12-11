import Data.List (sortOn)
import System.Environment (getArgs)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

type Point = (Int, Int)

lineToPoint :: String -> Point
lineToPoint s = (x, y)
  where [(x, rest)] = readsPrec 1 s
        [(y, _)]    = readsPrec 1 (drop 1 rest)

areaWithCorners :: Point -> Point -> Int
areaWithCorners (x, y) (x', y') = (abs (x - x') + 1) *  (abs (y - y') + 1)

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  contents <- readFile fileName
  let points = map lineToPoint (lines contents)
  print $ (uncurry areaWithCorners . last . sortOn (uncurry areaWithCorners) . pairs) points
