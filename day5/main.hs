import System.Environment

stringToRange :: String -> Range
stringToRange s = (read left, (read . drop 1) right)
  where (left, right) = span (/= '-') s

isInRanges :: [Range] -> Int -> [Range]
isInRanges ranges n = filter f ranges
  where f (lo,hi) = n <= hi && n >= lo

extractInputInfo :: [String] -> ([Range], [Int])
extractInputInfo lns = (optimizeRanges . map stringToRange $ rangeStrings, ids)
  where (rangeStrings, rest) = break null lns
        ids = map read (drop 1 rest)

type Range = (Int, Int)

-- add a new range to existing range blocks.
insertNewRange :: [Range] -> Range -> [Range]
insertNewRange [] range = [range]
insertNewRange (oldRange:ranges) newRange = case combineRanges newRange oldRange of
  LeftOf -> newRange : oldRange : ranges
  RightOf -> oldRange : insertNewRange ranges newRange
  OverlapComplete range -> range : ranges
  OverlapLeft range -> range : ranges
  OverlapRight range -> insertNewRange ranges range

data RangeRelation = OverlapRight Range | OverlapLeft Range | OverlapComplete Range | LeftOf | RightOf deriving Show

combineRanges :: Range -> Range -> RangeRelation
combineRanges (newLo, newHi) (currentLo, currentHi)
  -- new range is completely to the right of the current range
  | newLo > currentHi = RightOf
  -- new range is completely to the left of the current range
  | newHi < currentLo = LeftOf
  -- new range is completely inside the current one
  | newLo >= currentLo && newHi <= currentHi = OverlapComplete (currentLo, currentHi) 
  -- new range completely covers the current one
  | newHi >= currentHi && newLo <= currentLo = OverlapComplete (newLo, newHi)
  -- new range overlaps old on the left
  | newLo <= currentLo && newHi <= currentHi && currentLo <= newHi = OverlapLeft (newLo, currentHi)
  -- new range overlaps old on the right
  | newHi >= currentHi && newLo >= currentLo && currentHi >= newLo = OverlapRight (currentLo, newHi)

optimizeRanges :: [Range] -> [Range]
optimizeRanges = absorb . foldr (flip insertNewRange) []

sumRanges :: [Range] -> Int
sumRanges = sum . map rng
  where rng (lo, hi) = (hi-lo)+1

absorb :: [Range] -> [Range]
absorb [] = []
absorb [r] = [r]
absorb (frst@(lo,hi):scnd@(lo',hi'):rest) = if hi == lo' then (lo,hi') : absorb rest else frst : scnd : absorb rest

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  (ranges, ids) <- extractInputInfo . lines <$> readFile fileName
  let fresh = not . null . isInRanges ranges
  let freshIDsCount = (length . filter fresh) ids
  let totalValidIDsCount = sumRanges ranges
  print (freshIDsCount, totalValidIDsCount)
