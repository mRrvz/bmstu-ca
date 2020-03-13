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

    putStrLn "Введите X:"
    x0 <- fmap toDouble getLine

    putStr "Результат вычислений: "
    print $ spline table x0
