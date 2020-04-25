module Main where


import System.IO


main :: IO ()
main = do
    tau <- putStrLn "Enter tau: " >> fmap (\x -> read x :: Double) getLine
    print tau
