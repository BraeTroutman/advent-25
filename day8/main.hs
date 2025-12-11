import System.Environment (getArgs)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array ((!), (//), Array (..), listArray, elems, bounds)
import Control.Monad.State
import Data.List (sortOn, sort, sortBy)
import Data.Ord (Down (..), comparing)

data Point = Point { x :: Float
                   , y :: Float
                   , z :: Float
                  } deriving (Show, Eq, Ord)

type Circuit = Set.Set Point

-- |structure threading state through the whole computation
data ProgState = ProgState {
                           -- |The mapping of a Circuit ID to the Circuit (Set) of Points corresponding to that Circuit ID
                             _sets :: Array Int Circuit
                           -- |Mapping of Points to their corresponding Circuit ID
                           , _pointToSetID :: Map.Map Point Int
                           -- |The ID of the largest Circuit created so far
                           , _largestSetId :: Int
                          } deriving Show

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  contents <- readFile fileName
  let points = map lineToPoint (lines contents)
  let initial = initState points
  let allPairs = sortOn (uncurry distance) (pairs points)
  let firstThousand = take 1000 allPairs
  let sltn1FinalState = execState (mapM step firstThousand) initial
  -- the pair that joined everything into one Circuit will be the first `Just` value in the list of outputs
  let (Just (Point x _ _, Point x' _ _)) = head . dropWhile (==Nothing) . evalState (mapM step allPairs) $ initial
  let sltn1 = (product . take 3 . sortBy (comparing Data.Ord.Down) . elems . fmap Set.size . _sets) sltn1FinalState
  print (sltn1, round x * round x')

-- |Updates the state after joining the Circuits of two points
updateStateWithPoints :: (Point, Point) -> ProgState -> ProgState
updateStateWithPoints (p1, p2) (ProgState sets pointToSetIdMap currentLargestSet) = ProgState newSets newMap newLargestSet
      where circuitIdOf = (pointToSetIdMap Map.!)
            s1 = circuitIdOf p1
            s2 = circuitIdOf p2
            -- union the Circuits the two points belong to if they're not already in the same circuit
            newSets = if s1 == s2 then sets else sets // [(s1, Set.union (sets ! s1) (sets ! s2)), (s2, Set.empty)]
            -- update the Circuit ID of all the points in the second Point's Circuit if they are joined
            newMap  = if s1 == s2 then pointToSetIdMap else setIdOfPointsTo (Set.toList $ sets ! s2) s1
            -- update the largest set value, if the new set formed by this union makes a larger one than the current max
            newLargestSet
              | Set.size (newSets ! s1) > Set.size (newSets ! currentLargestSet) = s1
              | otherwise = currentLargestSet
            setIdOfPointsTo pts setId = foldr (`Map.insert` setId) pointToSetIdMap pts

-- |Stateful computation that updates the program state with the result of connecting the two points.
-- returns Nothing until the point added joins every Point into one Circuit, in which case it returns that pair of points
step :: (Point, Point) -> State ProgState (Maybe (Point, Point))
step pt = do
  modify (updateStateWithPoints pt)
  ProgState sets _ maxSetId <- get
  let (_, n) = bounds sets
  -- we halt when when the size of largest Circuit is equal to the number of points (i.e. it contains all the points)
  if Set.size (sets ! maxSetId) == n then return (Just pt) else return Nothing

mkCircuit :: Point -> Circuit
mkCircuit = Set.singleton

-- |We start initially with a single Circuit for each point
initState :: [Point] -> ProgState
initState pts = ProgState (listArray (1, length pts) (map mkCircuit pts)) (Map.fromList $ zip pts [1..]) 1

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
