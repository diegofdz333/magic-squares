import Data.Array
import Data.Set (Set, toList, fromList, delete, member)
import System.Environment (getArgs)
import Control.Parallel

magicConstant :: Int -> Int
magicConstant n = n * (n * n + 1) `div` 2

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

rowStep :: Int -> Int -> Int -> Int -> Array (Int, Int) Int -> Set Int -> Int
rowStep n pos step depth square rest = possibleRowSteps $ toList rest
    where
        possibleRowSteps [] = 0
        possibleRowSteps (p:ps)
            | pos == n - 1                = lastRow
            | depth < targetD  = pVal `par` possibleRowSteps ps `pseq` (pVal + possibleRowSteps ps)
            | otherwise                   = pVal + possibleRowSteps ps
            where
                pVal
                    | step == n - 2 - pos = finalStep p
                    | otherwise           = rowStep n pos (step + 1) (depth + 1) nextSquare $ delete p rest
                nextSquare = square // [((pos, step + pos), p)]
        finalStep p = pVal
            where
                constant = magicConstant n
                base = constant - sum [ square ! (pos, j) | j <- [0..(n - 3)] ]
                p0 = square ! (0, 0)
                r = base - p
                s = delete p rest
                pVal
                    | pos == 0 && r < p0 = 0
                    | member r s         = colStep n pos 0 (depth + 1) nextSquare $ delete r s
                    | otherwise          = 0
                nextSquare = square // [((pos, n - 2), p), ((pos, n - 1), r)]
        lastRow = pVal
            where
                constant = magicConstant n
                r = constant - sum [ square ! (pos, j) | j <- [0..(n - 2)] ]
                pVal
                    | member r rest = if check n newSquare then 1 else 0
                    | otherwise  = 0
                    where
                        newSquare  = square // [((pos, n - 1), r)]

colStep :: Int -> Int -> Int -> Int -> Array (Int, Int) Int -> Set Int -> Int
colStep n pos step depth square rest = possibleColSteps $ toList rest
    where
        possibleColSteps [] = 0
        possibleColSteps (p:ps)
            | pos == n - 2     = lastCol
            | depth < targetD  = pVal `par` possibleColSteps ps `pseq` (pVal + possibleColSteps ps)
            | otherwise        = pVal + possibleColSteps ps
            where
                pVal
                    | step == n - 3 - pos = finalStep p
                    | otherwise           = colStep n pos (step + 1) (depth + 1) nextSquare $ delete p rest
                nextSquare = square // [((pos + 1 + step, pos), p)]
        finalStep p = pVal
            where
                constant = magicConstant n
                base = constant - sum [ square ! (i, pos) | i <- [0..(n-3)] ]
                s1 = square ! (0, n - 1)
                r = base - p
                s = delete p rest
                pVal
                    | pos == 0 && r < s1 = 0
                    | member r s         = rowStep n (pos + 1) 0 (depth + 1) nextSquare $ delete r s
                    | otherwise          = 0
                nextSquare = square // [((n - 2, pos), p), ((n - 1, pos), r)]
        lastCol = pVal
            where
                constant = magicConstant n
                r = constant - sum [ square ! (i, pos) | i <- [0..(n-2)] ]
                pVal
                    | member r rest = rowStep n (pos + 1) 0 (depth + 1) nextSquare $ delete r rest
                    | otherwise     = 0
                    where
                        nextSquare  = square // [((n - 1, pos), r)]

enumerateSquares :: Int -> Int
enumerateSquares n = rowStep n 0 0 0 square values * 8
    where
        square = array ((0,0), (n-1, n-1)) [((i, j), 0) | i <- [0..n-1], j <- [0..n-1]]
        values = fromList [1..n*n]

targetD :: Int
targetD = 3

main :: IO ()
main = do
    a <- getArgs
    case a of
        [s] | n >= 1 -> print (enumerateSquares n) where n = read s :: Int
        _            -> putStrLn "one argument, must be integer >= 1"
