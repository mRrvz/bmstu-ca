import Interpolation
import Parse
import System.IO

main :: IO ()
main = do
    handle <- openFile "table.csv" ReadMode
    content <- hGetContents handle
    let table = parseTable $ lines content
    mapM_ print table
    hClose handle

    putStrLn "Введите x, y:"
    point <- fmap words getLine
    putStrLn "Введите Xn, Yn:"
    degrees <- fmap words getLine

    putStr "Результат вычислений: "
    print $ interpolation2 table (tuple point toDouble) (tuple degrees toInt)
