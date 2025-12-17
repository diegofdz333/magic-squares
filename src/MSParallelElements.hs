module MSParallelElements (enumerateSquares) where

import Data.Array
import Data.Set (Set, toList, fromList, delete, member)
import Control.Parallel

magicConstant :: Int -> Int
magicConstant n = n * (n * n + 1) `div` 2

-- Check if a filled Square is Magic
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

        -- Reflection / Rotation Fix
        checkLastCornerLargest = square ! (0, 0) < square ! (n - 1, n - 1)

-- Iterate step by step on a row. 
-- Pos is the index of the row in the square.
-- Step is the index within the row we are filling in (Without counting cells filled in previously by colStep)
-- Depth is used for parallelization
rowStep :: Int -> Int -> Int -> Int -> Array (Int, Int) Int -> Set Int -> Int
rowStep n pos step depth square rest = possibleRowSteps $ toList rest
    where
        possibleRowSteps [] = 0
        possibleRowSteps (p:ps)
            -- Our rules for Rotations / Reflections means the first element cannot be these values, prune early to reduce time & GC
            | pos == 0 && step == 0 && (p > n * n - 3) = 0
            | pos == n - 1     = lastRow -- The last row has some special properties
            | depth < targetD  = pVal `par` possibleRowSteps ps `pseq` (pVal + possibleRowSteps ps)
            | otherwise        = pVal + possibleRowSteps ps
            where
                pVal
                    | step == n - 2 - pos = finalStep p 
                    | otherwise           = rowStep n pos (step + 1) (depth + 1) nextSquare $ delete p rest
                nextSquare = square // [((pos, step + pos), p)]
        -- The final two elements of a row are done together
        -- This is because the last element has to be constant minues the sum of the rest, so no more recursion is needed
        finalStep p = pVal
            where
                constant = magicConstant n
                base = constant - sum [ square ! (pos, j) | j <- [0..(n - 3)] ]
                p0 = square ! (0, 0)
                r = base - p -- The necessary value for the last element for row to be magic.
                s = delete p rest
                pVal
                    | pos == 0 && r < p0 = 0 -- Reflection / Rotation Fix
                    | member r s         = colStep n pos 0 (depth + 1) nextSquare $ delete r s
                    | otherwise          = 0 -- Prune if the necessary last element of row does not exist.
                nextSquare = square // [((pos, n - 2), p), ((pos, n - 1), r)]
        -- The last row is actually just one element. Sum rest of row to figure its value then check if square is magic
        lastRow = pVal
            where
                constant = magicConstant n
                r = constant - sum [ square ! (pos, j) | j <- [0..(n - 2)] ]
                pVal
                    | member r rest = if check n newSquare then 1 else 0
                    | otherwise  = 0
                    where
                        newSquare  = square // [((pos, n - 1), r)]

-- Iterate step by step on a column. 
-- Pos is the index of the column in the square.
-- Step is the index within the column we are filling in (Without counting cells filled in previously by rowStep)
-- Depth is used for parallelization
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
                    | pos == 0 && r < s1 = 0 -- Reflection / Rotation Fix
                    | member r s         = rowStep n (pos + 1) 0 (depth + 1) nextSquare $ delete r s
                    | otherwise          = 0
                nextSquare = square // [((n - 2, pos), p), ((n - 1, pos), r)]
        -- The last column is actually just one element. Sum rest of column to figure its value
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
targetD = 2