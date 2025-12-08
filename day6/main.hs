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
  contents <- readFile fileName
  let sltn1 = sum . map solveProblem $ extractInputs contents
  let sltn2 = sum . map solveProblem $ extractInputsCephStyle contents
  print (sltn1, sltn2)

-- we need a function to split the numbers up that will be aware of the alignment of each number in
-- it's respective column
breakAwareSplit :: [String] -> [[String]]
breakAwareSplit [] = []
breakAwareSplit strings = block : breakAwareSplit (drop 1 rest)
  where (block, rest) = break (all (==' ')) strings 

extractInputsCephStyle :: String -> [Problem]
extractInputsCephStyle input = zipWith prependOp ops blocks
  where lns = lines input
        (blocks, ops) = (breakAwareSplit . transpose . init $ lns, filter (/=' ') . last $ lns)
        prependOp op block = [op] : block
