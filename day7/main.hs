import Control.Monad.Trans.State
import System.Environment (getArgs)
import Data.List
import qualified Data.Array as A

data Channel = Beam | Empty deriving Show

data ProgramState = ProgramState { beamLine :: [Channel]
                    , splitCount :: Int } deriving Show

type Idx = (Int, Int)

-- |Initialize the program state by creating the first BeamLine that will pass down the manifold.
initState :: String -> ProgramState
initState str = ProgramState (map chrToChannel str) 0
  where chrToChannel 'S' = Beam
        chrToChannel _   = Empty

-- |Convert a string into an array of characters, each row being a line.
--  note that this will fail in the case uneven line lengths
charArray :: String -> A.Array Idx Char
charArray content = A.listArray ((1,1),(rows,cols)) (filter (/='\n') content)
   where lns = lines content
         (rows, cols) = (length lns, length (head lns))

-- |DFS-like traversal of the possible Tachyon paths in the manifold. When we reach a splitter, we recur twice:
-- once into the left idx and once into the right. This is fine for small input sizes but this is exponential ultimately
-- (this is where we differ from true DFS, because if we were doing that we'd keep track of the nodes that we already visited)
dfs :: Idx -> A.Array Idx Char -> Int
dfs (row, col) arr = if row == rows then 1 else dispatch (arr A.! (row, col))
  where ((_, _), (rows, cols)) = A.bounds arr
        dispatch '^' = dfs (row, col-1) arr + dfs (row, col+1) arr
        dispatch _   = dfs (row+1, col) arr

-- |DFS-like traversal of the Tachyon paths, but with better management of avoiding recalculation of shared sub-problems.
-- The native lazy Array data structure is perfect for this, because we only calculate the entries we need, and the array can
-- recursively depend upon it's own content. Because we can reuse overlapping problems, this execution time is no longer exponential
-- in the number of splits
dfs' :: Idx -> A.Array Idx Char -> Int
dfs' startIdx arr = sltn A.! startIdx
  where ((_, _), (rows, cols)) = A.bounds arr
        sltn = A.array (A.bounds arr) [(ix, solveAtIdx ix val) | (ix, val) <- A.assocs arr]
        solveAtIdx (row, col) c = if row == rows then 1 else case c of
                                                              '^' -> sltn A.! (row, col-1) + sltn A.! (row, col+1)
                                                              _ -> sltn A.! (row+1, col)

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  content  <- readFile fileName
  let (ln1:lns) = lines content
  let arr = charArray content
  let startingIdx = case find ((=='S').snd) (A.assocs arr) of
                      Just (idx, _) -> idx
                      _ -> error "No starting index could be found"
  let sltn1 = last $ evalState (mapM updateCount lns) (initState ln1)
  let sltn2 = dfs' startingIdx arr
  print (sltn1, sltn2)

countSplits :: [(Channel, Char)] -> Int
countSplits = foldr f 0
  where f (Beam, '^') c = c + 1
        f _ c = c

-- |Look at each line of the manifold with the state of the previous, and update the count and new location
-- of the beams based on beams from the previous line and their interactions with splitters in this one
nextState :: [(Channel, Char)] -> [Channel]
nextState [] = []
nextState ((Beam, '.'):rest) = Beam : nextState rest
nextState ((Beam, '^'):(_, '.'):rest) = Empty : Beam : nextState rest
nextState ((Empty, '.'):(Beam, '^'):rest) = Beam : nextState ((Beam, '^'):rest)
nextState (_:rest) = Empty : nextState rest

-- |State step that takes a line and returns a state computation that evaluates what the count and beam line
-- should be based on the interaction between the current state and the new line
updateCount :: String -> State ProgramState Int
updateCount line = do
  (ProgramState beamLine countSoFar) <- get
  let rowState = zip beamLine line
  let newCount = countSoFar + countSplits rowState
  put $ ProgramState (nextState rowState) newCount
  return newCount
