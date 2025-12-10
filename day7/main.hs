import Control.Monad.Trans.State
import System.Environment (getArgs)
import Data.List

data Channel = Beam | Empty deriving Show

data ProgramState = ProgramState { beamLine :: [Channel]
                    , splitCount :: Int } deriving Show

initState :: String -> ProgramState
initState str = ProgramState (map chrToChannel str) 0
  where chrToChannel 'S' = Beam
        chrToChannel _   = Empty

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  (ln1:lns) <- lines <$> readFile fileName
  print $ last $ evalState (mapM updateCount lns) (initState ln1)

countSplits :: [(Channel, Char)] -> Int
countSplits = foldr f 0
  where f (Beam, '^') c = c + 1
        f _ c = c

nextState :: [(Channel, Char)] -> [Channel]
nextState [] = []
nextState ((Beam, '.'):rest) = Beam : nextState rest
nextState ((Beam, '^'):(_, '.'):rest) = Empty : Beam : nextState rest
nextState ((Empty, '.'):(Beam, '^'):rest) = Beam : nextState ((Beam, '^'):rest)
nextState (_:rest) = Empty : nextState rest

updateCount :: String -> State ProgramState Int
updateCount line = do
  (ProgramState beamLine countSoFar) <- get
  let rowState = zip beamLine line
  let newCount = countSoFar + countSplits rowState
  put $ ProgramState (nextState rowState) (newCount)
  return newCount
