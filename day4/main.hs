import Data.Array as A
import System.Environment

-- this is literally just a convolution lol, let me see if the matrix library has any stuff for convolutions + kernels
-- our kernel would be 111 to count how many rolls are around us, then we can just map over to count which ones have less than 4
--                     101
--                     111
-- eh ok no top level convolution function in Data.Matrix... I guess it would be in good spirit to implement it myself anyways
-- let's use 2D arrays, and we can define convolve ourselves

type Idx = (Int, Int)

type Bounds = (Idx, Idx)

type Matrix a = A.Array Idx a

data PrintBlock = Roll | Empty deriving (Eq, Show)

data BlockAccessibility = Accessible | NotAccessible | NA deriving (Eq, Show)

main :: IO ()
main = do
  fileName <- (!!0) <$> getArgs
  blockList <-  map (map charToBlock) . lines <$> readFile fileName
  let bnds = ((1,1), (length blockList, length $ head blockList))
  let blocks = listArray bnds (concat blockList)
  let sltn1 = count (==Accessible) $ map (accessibility blocks) (indices blocks)
  let sltn2 = cleanup blocks
  print (sltn1, sltn2)
  
count f = length . filter f

charToBlock :: Char -> PrintBlock
charToBlock '@' = Roll
charToBlock _ = Empty

blockToChar :: BlockAccessibility -> Char
blockToChar Accessible = 'x'
blockToChar NotAccessible = '@'
blockToChar NA = '.'

isOutOfBounds :: Bounds -> Idx -> Bool
isOutOfBounds ((rowMin,colMin),(rowMax,colMax)) (row, col) = row > rowMax || row < rowMin || col > colMax || col < colMin

neighborsWithDefault :: a -> Matrix a -> Idx -> [a]
neighborsWithDefault def mat (row,col) = map defaultEdge idxs
  where idxs = [(row-1,col-1), (row-1,col), (row-1,col+1)
               ,(row,  col-1),              (row,  col+1)
               ,(row+1,col-1), (row+1,col), (row+1,col+1)]
        bnds = bounds mat
        defaultEdge e = if isOutOfBounds bnds e then def else mat A.! e

accessibility :: Matrix PrintBlock -> Idx -> BlockAccessibility
accessibility mat idx = case val of
    Roll -> if count (==Roll) (neighborsWithDefault Empty mat idx) < 4 then Accessible else NotAccessible
    Empty -> NA
  where val = mat A.! idx
        count f = length . filter f

removeAccessible :: BlockAccessibility -> PrintBlock
removeAccessible NotAccessible = Roll
removeAccessible _ = Empty

cleanup :: Matrix PrintBlock -> Int
cleanup = cleanup' 0

cleanup' :: Int -> Matrix PrintBlock -> Int
cleanup' c mat = let
    accessibilityList = map (accessibility mat) (indices mat)
  in case count (==Accessible) accessibilityList of
    0 -> c
    n -> cleanup' (c+n) (nextState accessibilityList)
  where bnds = bounds mat
        nextState ls = listArray bnds (map removeAccessible ls)

