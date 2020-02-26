import Interpolation
import Parse
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
    handle <- openFile "table.txt" ReadMode
    content <- hGetContents handle
    let table = parseTable $ lines content
    mapM_ print (valueMatrix table)
    hClose handle

    point <- fmap words getLine
    degrees <- fmap words getLine
    putStr "Результат вычислений: "
    print $ multidemInterpolation table (toDoubleTuple point) (toIntTuple degrees)

    {-
    putStr "Результат вычислений: "
    case head findType of
      "bisection" -> print $ bisection table (read n)
      _ -> print $ newtonPolynomial table (read x) (read n)
    -}
