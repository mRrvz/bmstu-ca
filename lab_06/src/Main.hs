import System.IO
import Differentation

f :: [Double]
f = [0.571, 0.889, 1.091, 1.231, 1.333, 1.412]

x :: [Double]
x = [1..6]

main :: IO ()
main = do
    putStr "Left side: " >> print (leftSide f 1)
    putStr "Center:" >> print (centerDiff f 1)
    putStr "Runge: " >> print (rungeCenter f 1 1)
    putStr "Alignment variables: " >> print (alignment f x 1)
    putStr "Second derivative: " >> print (differential2 f 1)
