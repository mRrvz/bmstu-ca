import System.IO
import System.Environment
import Data.List
import Interpolation

main :: IO ()
main = do
    (x:n:findType) <- getArgs
    let (xs, ys) = initialConditions $ head findType
        approx = unzip $ takeApproximation (zip xs ys) (read x) (read n)
        table = createTable (fst approx) (snd approx) 0

    putStrLn "Результат: "
    print $ (interpolation (fst approx) table $ read x) + (head $ snd approx)
    putStrLn "Таблица: \n| x | y | Разделенные разности |"
    mapM_ print $ transpose $ fst approx : snd approx : table
