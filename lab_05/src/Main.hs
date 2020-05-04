module Main where

import System.IO
import Integration

main :: IO ()
main = do
    tau <- putStrLn "Enter tau: " >> fmap (\x -> read x :: Double) getLine
    n <- putStrLn "Enter N: " >> fmap (\x -> read x :: Int) getLine
    m <- putStrLn "Enter M: " >> fmap (\x -> read x :: Int) getLine
    mapM_ print [n, m]
    print $ simpson2 n m tau limits
    print $ gauss2 n m tau limits
    --print $ gauss m 0 (pi/2)
    --print $ getCoeffs k
