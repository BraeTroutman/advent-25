import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  idList <- concatMap stringToRange . lines <$> readFile fileName
  let invalidCount1 = sum . filter isTwoRepeatedDigits $ idList
  let invalidCount2 = sum . filter hasRepeatedNumbers $ idList
  putStrLn $ show invalidCount1 ++ " IDs satisfy first condition, and " ++ show invalidCount2 ++ " satisfy the second."

-- utility function for a few locations. We can use it to determine if a number can even have repeats
-- (number of digits must be even for that). It can also help us break up the number into two part for
-- comparison, allowing us to do a fast int compare instead of slower [Char] operations
numDigits :: Int -> Int
numDigits = succ . floor . logBase 10 . fromIntegral

-- extracts the n most significant digits in the number x
highestOrderNDigits :: Int -> Int -> Int
highestOrderNDigits n x = let c = numDigits x in if x == 0 then x else x `div` (10^(max 0 (c-n)))

-- use mod and div to split the num into two halves.
halves :: Int -> (Int, Int)
halves n = (left, right)
  where halfSize = numDigits n `div` 2
        (left, right) = n `divMod` (10^halfSize)

-- get the idx'd chunk of size from the number in question. If the number of digits in the lower half of the number
-- extracted to create the chunk is not equal to in size to the idx * size, that means we have leading zeros-- and there's
-- no way that this chunk will be equal to the last one, since we're guaranteed that the whole item ID won't start with leading
-- zeros. No other chunks will be negative naturally, so we encode this difference by returning negative 1
getChunk :: Int -> Int -> Int -> Int
getChunk num size idx = if numDigits lows == idx*size || idx == 1 then highestOrderNDigits size lows else -1
  where lows = num `mod` modFactor
        modFactor = 10^(idx*size)

-- split the number `n` into `chunks` parts
parts :: Int -> Int -> [Int]
parts chunks n = map (getChunk n chunkSize) [1..chunks]
  where chunkSize = numDigits n `div` chunks

isTwoRepeatedDigits :: Int -> Bool
isTwoRepeatedDigits n
  | even (numDigits n) = let (left, right) = halves n in left == right
  | otherwise = False

stringToRange :: String -> [Int]
stringToRange s = [read left..( read . tail $ right)]
  where (left, right) = span (/= '-') s

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

allEqual :: (Eq a) => [a] -> Bool
allEqual []     = False
allEqual [_]    = False
allEqual (x:xs) = all (==x) xs

hasRepeatedNumbers :: Int -> Bool
hasRepeatedNumbers n = any f facs
  where facs = factors (numDigits n)
        f i = let chunks = parts i n in allEqual chunks
