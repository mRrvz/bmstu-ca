module Main where

import System.IO
import Math.Polynomial.Legendre
--import Simpson
import Gauss


main :: IO ()
main = do
    tau <- putStrLn "Enter tau: " >> fmap (\x -> read x :: Double) getLine
    n <- putStrLn "Enter N: " >> fmap (\x -> read x :: Int) getLine
    m <- putStrLn "Enter M: " >> fmap (\x -> read x :: Int) getLine
    mapM_ print [n, m]
    --let limits =
    --print $ simpson n m limits
    let k = legendreRoots 3 0.001
    --print $ gaussSLE [[1, 1, 1, 2], [-0.77459, 0, 0.77459, 0], [0.7754 ^ 2, 0, 0.77459 ^ 2, 2 / 3]]
    print $ getCoeffs k
