import Control.Monad.Trans.State

data Direction = L | R deriving Show

data Turn = Turn Direction Int deriving Show

-- gotta keep our state somewhere
data ProgramState = ProgramState { zeroCount :: Int
                                 , location  :: Int
                                 } deriving Show

main :: IO ()
main = do
  turns <- map stringToTurn . lines <$> readFile "input.txt"
  -- calculate the hits and passes using the power of **MONADIC ACTIONS**
  let zeroHits = zeroCount $ execState (mapM countPointsToZero turns) initialState
  let zeroPasses = zeroCount $ execState (mapM countPassesOverZero turns) initialState
  putStrLn $ "We hit zero: " ++ show zeroHits ++ " times, and pass it: " ++ show zeroPasses ++ " times."

initialState :: ProgramState
initialState = ProgramState 0 50

-- convert a turn into an integer-- negative if going left, positive going right
turnToNum :: Turn -> Int
turnToNum (Turn L n) = (-n)
turnToNum (Turn R n) = n

-- jankily parse a move represented as a string
stringToTurn :: String -> Turn
stringToTurn ('L':n) = Turn L (read n :: Int)
stringToTurn ('R':n) = Turn R (read n :: Int)

calcMove :: Turn -> Int -> Int
calcMove turn loc = (loc + turnToNum turn) `mod` 100

countPointsToZero :: Turn -> State ProgramState Int
countPointsToZero turn = do
  programState <- get
  let newLocation = calcMove turn $ location programState
  let count = let c = zeroCount programState in if newLocation == 0 then c+1 else c
  put $ programState { location = newLocation, zeroCount = count}
  return count

countPassesOverZero :: Turn -> State ProgramState Int
countPassesOverZero turn = do
  pSt@(ProgramState zeros loc) <- get
  -- could probably more efficiently calculate the move and the zeros in the same pass, but I'm lazy
  let newLocation = calcMove turn loc
  let count = zeros + calcZeros turn pSt
  put $ pSt { location = newLocation, zeroCount = count }
  return count

calcZeros :: Turn -> ProgramState -> Int
-- rotating from 0 is a special case, we'll pass 0 exactly as many times as 100 goes into the rotation distance
calcZeros (Turn _ n) (ProgramState zeros 0) = n `div` 100
-- rotating from anywhere else but zero, we will pass 0 exactly as many times as 100 goes into the rotation distance,
-- PLUS we'll hit it once more if the remainder of n / 100 is greater than or equal to our locations distance from 0
-- in the turn's direction. If the remainder is equal to that distance, we land on zero exactly. Otherwise we pass it.
calcZeros (Turn L n) (ProgramState zeros loc) = let (times, rem) = n `divMod` 100 in if rem >= loc then times + 1 else times
calcZeros (Turn R n) (ProgramState zeros loc) = let (times, rem) = n `divMod` 100 in if rem >= (100 - loc) then times + 1 else times
