module Main where


import System.IO


main :: IO ()
main = do
    tau <- putStrLn "Enter tau: " >> fmap (\x -> read x :: Double) getLine
    n <- putStrLn "Enter N: " >> fmap (\x -> read x :: Int) getLine
    m <- putStrLn "Enter M: " >> fmap (\x -> read x :: Int) getLine
    mapM_ print [n, m]
