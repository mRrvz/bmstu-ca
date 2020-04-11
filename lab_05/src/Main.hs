import Parse
import Gauss
import Plot
import Approximation
import System.IO

printRow :: ((Double, Double), Double) -> IO ()
printRow row = putStrLn $ show (fst $ fst row) ++ " " ++ show (snd $ fst row) ++ " " ++ show (snd row)

main :: IO ()
main = do
    table <- openFile "table.csv" ReadMode >>= hGetContents >>= return . parseTable . lines
    putStrLn "X   Y   P" >> mapM_ printRow (zip (xy table) $ weight table) >> putStrLn "Enter n:"
    coeffs <- fmap toInt getLine >>= return . (+ 1) >>= return . quadraticApproximation table
    print coeffs
    plotApproximation (f coeffs) $ xy table
