import System.IO
import Interpolation
import Data.List

main :: IO ()
main = do
    putStrLn "Результат: "
    print $ (interpolation (fst approx) table 1.5) + (head $ snd approx)
    putStrLn "Таблица: \n| x | y | Разделенные разности |"
    mapM_ print (transpose $ fst approx : snd approx : table)
        where
            xs = [1..20]
            ys = [x * x | x <- xs]
            approx = unzip $ takeApproximation (zip xs ys) 1.5 7
            table = createTable (fst approx) (snd approx) 0
