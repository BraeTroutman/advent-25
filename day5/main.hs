import System.Environment

type Range = (Int, Int)

-- encode the relationship between two ranges as an enum for clear case handling
data RangeRelation = OverlapRight Range | OverlapLeft Range | OverlapComplete Range | LeftOf | RightOf deriving Show

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

-- if we build our list of ranges in this ordered way, absorbing overlapping ranges as we go, we
-- don't have to worry about repeating overlapped range values when we count the valid IDs
-- this is essentially a modified sorted insertion, with additional neighbor-absorbing logic
-- add a new range to existing range blocks.
insertNewRange :: [Range] -> Range -> [Range]
insertNewRange [] range = [range]
insertNewRange (oldRange:ranges) newRange = case combineRanges newRange oldRange of
  -- if the new range is completely to the left of the next, then we can just put it ahead of the next and stop recurring
  LeftOf -> newRange : oldRange : ranges
  -- if it's completely to the right of the next, we need to keep bubbling it up until we reach the right range
  RightOf -> oldRange : insertNewRange ranges newRange
  -- if the overlap is complete, we prepend the overlapped range
  OverlapComplete range -> range : ranges
  -- same here
  OverlapLeft range -> range : ranges
  -- we need to keep bubbling up the new range (at most one more time), because the upper range of our new could
  -- still overlap with the next
  OverlapRight range -> insertNewRange ranges range

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

-- for some reason I'm hitting some edge case where two ranges that have an equal high and low value respectively don't
-- absorb into eachother on insertion... I can't find out why, but to cover that case I added this function to make one
-- final pass over the ranges to merge any adjactent ranges that share a hi/lo
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
