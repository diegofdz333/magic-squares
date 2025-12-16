import Data.Array
import Data.List (permutations)
import Data.Set (Set, toList, fromList, delete, member)
import qualified Data.Set as S
import System.Environment (getArgs)
import Control.Parallel

-- index by index
-- 

-- 4 = 3:30
-- 4 = 1:50s
-- 5 secs??!

-- 0.25 seconds in fast computer

-- we have half an hour for the presentation (but 15-20 is probably better)

magicConstant :: Int -> Int
magicConstant n = n * (n * n + 1) `div` 2

lengthNPermutations :: Set a -> Int -> [[a]]
lengthNPermutations xs n = concatMap permutations $ subsequencesOfLength n $ toList xs
    where
        subsequencesOfLength :: Int -> [a] -> [[a]]
        subsequencesOfLength 0 _      = [[]]
        subsequencesOfLength _ []     = []
        subsequencesOfLength k (y:ys) = map (y:) (subsequencesOfLength (k-1) ys) ++ subsequencesOfLength k ys

check :: Int -> Array (Int, Int) Int -> Bool
check n square = checkRows 0 && checkCols 0 && checkDiagOne && checkDiagTwo && checkLastCornerLargest
    where
        constant = magicConstant n

        checkRows i
            | i == n    = True
            | otherwise = checkRow i && checkRows (i + 1)
        checkRow i = sum [ square ! (i, j) | j <- [0..(n-1)] ] == constant

        checkCols i
            | i == n    = True
            | otherwise = checkCol i && checkCols (i + 1)
        checkCol j = sum [ square ! (i, j) | i <- [0..(n-1)] ] == constant

        checkDiagOne = sum [ square ! (i, i) | i <- [0..(n-1)] ] == constant

        checkDiagTwo = sum [ square ! (n - i - 1, i) | i <- [0..(n-1)] ] == constant

        -- For reflextion fixes
        checkLastCornerLargest = square ! (0, 0) < square ! (n - 1, n - 1)

row :: Int -> Int -> Array (Int, Int) Int -> Set Int -> Int
row n pos square rest = possibleRows stepPerms
    where
        constant = magicConstant n
        base = constant - sum [ square ! (pos, j) | j <- [0..(pos - 1)] ]
        stepPerms = lengthNPermutations rest (n - 1 - pos)
        possibleRows []     = 0
        possibleRows (p:ps)
            | pos * 2 < targetD = pVal `par` possibleRows ps `pseq` (pVal + possibleRows ps)
            | otherwise         = pVal + possibleRows ps
            where
                p0 = case p of
                    (p0':_)   -> p0'
                    []        -> error "tst"                
                r = base - sum p
                s = foldr delete rest p
                pVal
                    | pos == 0 && r < p0 = 0
                    | member r s         = col n pos nextSquare $ delete r s
                    | otherwise          = 0
                nextSquare = square // [((pos, j), v) | (j, v) <- zip [pos..] p]
                                    // [((pos, n - 1), r)]

col :: Int -> Int -> Array (Int, Int) Int -> Set Int -> Int
col n pos square rest
    | S.null rest = if check n square then 1 else 0
    | otherwise     = possibleCols stepPerms
    where
        constant = magicConstant n
        base = constant - sum [ square ! (i, pos) | i <- [0..pos] ]
        stepPerms = lengthNPermutations rest (n - 2 - pos)
        possibleCols []     = 0
        possibleCols (p:ps)
            | pos * 2 + 1 < targetD = pVal `par` possibleCols ps `pseq` (pVal + possibleCols ps)
            | otherwise             = pVal + possibleCols ps
            where
                s1 = square ! (0, n - 1)
                r = base - sum p
                s = foldr delete rest p
                pVal
                    | pos == 0 && r < s1 = 0
                    | member r s                     = row n (pos + 1) nextSquare $ delete r s
                    | otherwise                      = 0
                nextSquare = square // [((i, pos), v) | (i, v) <- zip [pos+1..] p]
                                    // [((n - 1, pos), r)]


enumerateSquares :: Int -> Int
enumerateSquares n = row n 0 square values * 8
    where
        square = array ((0,0), (n-1, n-1)) [((i, j), 0) | i <- [0..n-1], j <- [0..n-1]]
        values = fromList [1..n*n]

targetD :: Int
targetD = 1

main :: IO ()
main = do
    a <- getArgs
    case a of
        [s] | n >= 1 -> print (enumerateSquares n) where n = read s :: Int
        _            -> putStrLn "one argument, must be integer >= 1"
