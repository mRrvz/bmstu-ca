module Main where

import System.IO
import Integration
import Plot

main :: IO ()
main = do
    tau <- putStrLn "Enter tau: " >> fmap (\x -> read x :: Double) getLine
    n <- putStrLn "Enter N: " >> fmap (\x -> read x :: Int) getLine
    m <- putStrLn "Enter M: " >> fmap (\x -> read x :: Int) getLine
    print $ simpson2 n m tau limits
    print $ gauss2 n m tau limits
