import Parse
import Gauss
import Plot
import Approximation
import System.IO

f :: [Double] -> Double -> Double
f coeffs x = sum $ zipWith (*) coeffs (map (\y -> x ^ y) [0..length coeffs - 1])

main :: IO ()
main = do
    handle <- openFile "table.csv" ReadMode
    content <- hGetContents handle
    let table = parseTable $ lines content
    --mapM_ print table
    hClose handle

    putStrLn "Enter n:"
    n <- fmap toInt getLine
    let table = [[1, 1], [2, 2], [3, 1], [4, 2], [5, 1], [6, 2]]
    let w = take 6 $ repeat 1
    --print $ gauss [[5, 7, 6, 11], [3, 16, 19, 13], [13, 10, 7, 12]]
    print $ quadraticApproximation table w $ n + 1
    let coeffs = quadraticApproximation table w $ n + 1
    plotApproximation (f coeffs) [(1,1), (2,2), (3, 1), (4, 2), (5, 1), (6, 2)]

    --putStr "Result: "
    --print $ spline table x0
