import System.IO
import Differentation

main :: IO ()
main = do
    print $ leftSide [0.571, 0.889, 1.091, 1.231, 1.333, 1.412] 1
    print $ rightSide [0.571, 0.889, 1.091, 1.231, 1.333, 1.412] 1
    print $ centerDiff [0.571, 0.889, 1.091, 1.231, 1.333, 1.412] 1
    print $ rungeCenter [0.571, 0.889, 1.091, 1.231, 1.333, 1.412] 1 1
    print $ alignment [1..6] [0.571, 0.889, 1.091, 1.231, 1.333, 1.412]
    print $ alignment [1..6] $ map (\x -> x^3) [1..6]
    print $ differential2 (map (\x -> x^3) [1..6]) 1
