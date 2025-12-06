import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Ord (comparing)


main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  lines <- lines <$> readFile fileName
  let sol1 = sum . map score $ lines
  let sol2 = sum . map score' $ lines
  putStrLn $ "Solution 1: " ++ show sol1 ++ " . Solution 2: " ++ show sol2

-- naively generate the possible combinations of batteries in a bank
combinations :: [a] -> [(a,a)]
combinations [] = []
combinations (x:xs) = [(x, y) | y <- xs] ++ combinations xs

pairToInt :: (Char, Char) -> Int
pairToInt (a, b) = read [a, b]

score :: String -> Int
score = maximum . map pairToInt . combinations

-- first, we choose the largest digit in the first |n| - 12 batteries in the bank, and add that to our number
-- then we take everything after that digit, and do the same thing, but with |n| - 11 instead
-- repeat until 0
buildHighestString :: String -> String -> Int -> String
buildHighestString soFar _ 0 = soFar
buildHighestString soFar xs n = buildHighestString (soFar ++ [digit]) (drop idx xs) (n-1)
  where left = take (length xs - (n - 1)) xs
        (idx, digit) = maximumBy (comparing snd) $ reverse $ zip [1..] left

score' :: String -> Int
score' s = read $ buildHighestString "" s 12
