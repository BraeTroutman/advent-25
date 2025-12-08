import Data.List (transpose)
import System.Environment

type Problem = [String]

extractInputs :: String -> [Problem]
extractInputs = map reverse . transpose . map words . lines

solveProblem :: Problem -> Int
solveProblem ("*":rest) = product $ map read rest
solveProblem ("+":rest) = sum $ map read rest

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  problems <- extractInputs <$> readFile fileName
  let sltn1 = sum . map solveProblem $ problems
  print sltn1
