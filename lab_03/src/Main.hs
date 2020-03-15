import Parse
import Spline
import System.IO

main :: IO ()
main = do
    handle <- openFile "table.csv" ReadMode
    content <- hGetContents handle
    let table = parseTable $ lines content
    mapM_ print table
    hClose handle

    putStrLn "Enter X:"
    x0 <- fmap toDouble getLine

    putStr "Result: "
    print $ spline table x0
