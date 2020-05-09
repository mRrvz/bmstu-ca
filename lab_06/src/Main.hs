import System.IO
import Text.Printf
import Differentation

f :: [Double]
f = [0.571, 0.889, 1.091, 1.231, 1.333, 1.412]

x :: [Double]
x = [1..6]

main :: IO ()
main = do
    putStrLn "!!! Results are not balanced (by x value) !!!"
    putStr "\nLeft side: " >> mapM_ (printf "%.4f ") (leftSide f 1)
    putStr "\nCenter: " >> mapM_ (printf "%.4f ") (centerDiff f 1)
    putStr "\nRunge: " >> mapM_ (printf "%.4f ") (rungeCenter f 1 1)
    putStr "\nAlignment variables: " >> mapM_ (printf "%.4f ") (alignment f x 1)
    putStr "\nSecond derivative: " >> mapM_ (printf "%.4f ") (differential2 f 1)
