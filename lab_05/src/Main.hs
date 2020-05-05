import System.IO
import Integration

main :: IO ()
main = do
    tau <- putStrLn "Enter tau: " >> fmap (\x -> read x :: Double) getLine
    n <- putStrLn "Enter N: " >> fmap (\x -> read x :: Int) getLine
    putStrLn "Enter M: " >> fmap (\x -> read x :: Int) getLine >>= print . gauss2 limits tau n
