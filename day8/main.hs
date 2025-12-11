import System.Environment (getArgs)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array ((!), (//), Array (..), listArray, elems)
import Control.Monad.State
import Data.List (sortOn, sort, sortBy)
import Data.Ord (Down (..), comparing)

data Point = Point { x :: Float
                   , y :: Float
                   , z :: Float
                  } deriving (Show, Eq, Ord)

type Circuit = Set.Set Point

data ProgState = ProgState { _sets :: Array Int Circuit, _pointToSetID :: Map.Map Point Int} deriving Show

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  contents <- readFile fileName
  let points = map lineToPoint (lines contents)
  let initial = initState points
  let pairs' = take 1000 $ sortOn (uncurry distance) (pairs points)
  let finalState = execState (mapM step pairs') initial
  let sltn = (product . take 3 . sortBy (comparing Data.Ord.Down) . elems . fmap Set.size . _sets) finalState
  print sltn

updateStateWithPoints :: (Point, Point) -> ProgState -> ProgState
updateStateWithPoints (p1, p2) (ProgState sets pointToSetIdMap) = ProgState newSets newMap
      where setOf = (pointToSetIdMap Map.!)
            s1 = setOf p1
            s2 = setOf p2
            newSets = if s1 == s2 then sets else sets // [(s1, Set.union (sets ! s1) (sets ! s2))]
            newMap  = if s1 == s2 then pointToSetIdMap else setIdOfPointsTo (Set.toList $ sets ! s2) s1
            setIdOfPointsTo pts setId = foldr (`Map.insert` setId) pointToSetIdMap pts

step :: (Point, Point) -> State ProgState ()
step pt = modify (updateStateWithPoints pt)

mkCircuit :: Point -> Circuit
mkCircuit = Set.singleton

initState :: [Point] -> ProgState
initState pts = ProgState (listArray (1, length pts) (map mkCircuit pts)) (Map.fromList $ zip pts [1..])

distance :: Point -> Point -> Float
distance (Point x y z) (Point x' y' z') = sqrt $ (x - x')^2 + (y - y')^2 + (z - z')^2

lineToPoint :: String -> Point
lineToPoint str = Point x y z
  where [(x, r)] = readsPrec 1 str
        [(y, r')] = readsPrec 1 $ drop 1 r
        [(z, r'')] = readsPrec 1 $ drop 1 r'

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs
