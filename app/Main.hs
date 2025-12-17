module Main (main) where

import System.Environment (getArgs)

import qualified MSSequential as Seq1
import qualified MSSequentialRR as Seq2
import qualified MSParallel as Par1
import qualified MSParallelElements as Par2

main :: IO ()
main = do
    a <- getArgs
    case a of
        [mode, nStr] | n >= 1 -> print (magicSquares mode n) where n = read nStr :: Int
        _            -> putStrLn "Usage: stack run <mode> <n>"
        where
            magicSquares mode n
                | mode == "seq1" = Seq1.enumerateSquares n
                | mode == "seq2" = Seq2.enumerateSquares n
                | mode == "par1" = Par1.enumerateSquares n
                | mode == "par2" = Par2.enumerateSquares n
                | otherwise      = error "<mode> must be \"par1\""