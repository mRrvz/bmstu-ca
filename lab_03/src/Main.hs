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

    putStrLn "Введите X0 и искомый Y: "
    x0 <- fmap toDouble getLine

    putStr "Результат вычислений: "
    print $ spline table x0
    --mapM_ print $ map length (spline table point)
    --print $ interpolation2 table (tuple point toDouble) (tuple degrees toInt)
