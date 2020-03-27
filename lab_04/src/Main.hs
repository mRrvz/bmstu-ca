import Parse
import Gauss
import System.IO

main :: IO ()
main = do
    handle <- openFile "table.csv" ReadMode
    content <- hGetContents handle
    let table = parseTable $ lines content
    --mapM_ print table
    hClose handle

    --putStrLn "Enter n:"
    --n <- fmap toDouble getLine
    print $ gauss [[5, 7, 6, 11], [3, 16, 19, 13], [13, 10, 7, 12]]

    --putStr "Result: "
    --print $ spline table x0
