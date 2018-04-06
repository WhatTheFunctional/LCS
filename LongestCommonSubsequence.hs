--LongestCommonSubsequence.hs
--Copyright Laurence Emms 2018
--LCS implementation

import System.Environment
import Data.Array((!))
import qualified Data.Array
import qualified Data.Matrix

data Direction = NoDir | LeftDir | UpDir | DiagonalDir deriving (Show, Eq)

--Function to initialize matrix
initMatrix :: Int -> Int -> Data.Matrix.Matrix (Direction, Int)
initMatrix a b = Data.Matrix.matrix a b (\(i, j) -> (NoDir, 0))

updateMatrix :: Direction -> Int -> Int -> Data.Array.Array Int Char -> Data.Array.Array Int Char -> Data.Matrix.Matrix (Direction, Int) -> Data.Matrix.Matrix (Direction, Int)
updateMatrix NoDir i j a b m = if (a ! (i - 1)) == (b ! (j - 1))
                               then let diagM = lcsMatrix (i - 1) (j - 1) a b m
                                        (diagDir, diagValue) = Data.Matrix.getElem i j diagM
                                    in Data.Matrix.setElem (DiagonalDir, (1 + diagValue)) (i + 1, j + 1) diagM
                               else let leftM = lcsMatrix (i - 1) j a b m
                                        upM = lcsMatrix i (j - 1) a b leftM
                                        (leftDir, leftValue) = Data.Matrix.getElem (i + 1) j upM
                                        (upDir, upValue) = Data.Matrix.getElem i (j + 1) upM
                                    in if leftValue < upValue
                                       then Data.Matrix.setElem (UpDir, upValue) (i + 1, j + 1) upM
                                       else Data.Matrix.setElem (LeftDir, leftValue) (i + 1, j + 1) upM
updateMatrix _ _ _ _ _ m = m --Matrix already has a value here

--Function to compute LCS matrix
lcsMatrix :: Int -> Int -> Data.Array.Array Int Char -> Data.Array.Array Int Char -> Data.Matrix.Matrix (Direction, Int) -> Data.Matrix.Matrix (Direction, Int)
lcsMatrix 0 0 _ _ m = m
lcsMatrix 0 _ _ _ m = m
lcsMatrix _ 0 _ _ m = m
lcsMatrix i j a b m = let (thisDir, thisValue) = Data.Matrix.getElem (i + 1) (j + 1) m
                      in updateMatrix thisDir i j a b m

traceMatrix :: Int -> Int -> Data.Array.Array Int Char -> Data.Array.Array Int Char -> Data.Matrix.Matrix (Direction, Int) -> [Char] -> [Char]
traceMatrix i 1 a b _ longestCommonSubsequence = if a ! (i - 1) == b ! 0
                                                 then a ! (i - 1) : longestCommonSubsequence
                                                 else longestCommonSubsequence
traceMatrix 1 j a b _ longestCommonSubsequence = if a ! 0 == b ! (j - 1)
                                                 then a ! 0 : longestCommonSubsequence
                                                 else longestCommonSubsequence
traceMatrix i j a b m longestCommonSubsequence = let (direction, _) = Data.Matrix.getElem (i + 1) (j + 1) m
                                                 in case direction of
                                                    LeftDir -> traceMatrix i (j - 1) a b m longestCommonSubsequence
                                                    UpDir -> traceMatrix (i - 1) j a b m longestCommonSubsequence
                                                    DiagonalDir -> traceMatrix (i - 1) (j - 1) a b m (a ! (i - 1) : longestCommonSubsequence)

--Function to compute longest common subsequence
lcs :: Data.Array.Array Int Char -> Data.Array.Array Int Char -> Data.Matrix.Matrix (Direction, Int) -> IO ()
lcs a b m
    = putStrLn (traceMatrix (length a - 1) (length b - 1) a b subProblemMatrix "")
    -- = putStrLn (show subProblemMatrix)
        where subProblemMatrix = lcsMatrix (length a - 1) (length b - 1) a b m

main = getArgs >>= (\args ->
       case args of
       [] -> putStrLn "Please enter two strings for lcs"
       (a : []) -> putStrLn "Please enter two strings for lcs"
       --(a : b : []) -> let arrayA = Data.Array.listArray (0, length a) a
                           --arrayB = Data.Array.listArray (0, length b) b
                       --in putStrLn (show (initMatrix arrayA arrayB)))
       (a : b : []) -> let arrayA = Data.Array.listArray (0, length a) a
                           arrayB = Data.Array.listArray (0, length b) b
                       in lcs arrayA arrayB (initMatrix (length arrayA) (length arrayB)))
