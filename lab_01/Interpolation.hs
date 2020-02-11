module Interpolation (
    initialConditions,
    takeApproximation,
    createTable,
    newtonPolynomial
) where

import Data.List
import Data.Maybe
import Data.Sort

initialConditions :: [Char] -> ([Double], [Double])
initialConditions findType
    | findType == "intpol"  = (xs, ys)
    | findType == "back-intpol" = unzip $ sortOn fst $ zip ys xs
    where xs = [1..20]
          ys = [x * x| x <- xs]

slice :: [(Double, Double)] -> Int -> Int -> [(Double, Double)]
slice table n pos = take n $ drop pos table

takeApproximation :: [(Double, Double)] -> Double -> Int -> [(Double, Double)]
takeApproximation table x0 n
    | (<=) x0 . fst $ head table = take n table
    | (>=) x0 . fst $ last table = reverse $ take n $ reverse table
    | otherwise = left ++ right
        where indexL = fromJust $ findIndex (\x -> fst x >= x0) table
              left = slice table (n `div` 2 ) (indexL - n `div` 2)
              indexR = fromJust $ findIndex (== last left) table
              right = slice table (n - length left) (indexR + 1)

createTable :: [Double] -> [Double] -> Int -> [[Double]]
createTable _ (_:[]) _ = []
createTable xs ys step =  divDiff xs ys step : createTable xs (divDiff xs ys step) (step + 1)
    where divDiff _ (_:[]) _ = []
          divDiff xs ys step = (ys !! 1 - ys !! 0) / (xs !! (1 + step) - xs !! 0) : divDiff (tail xs) (tail ys) step

newtonPolynomial :: [Double] -> [[Double]] -> Double -> Double -> Double
newtonPolynomial xs table x0 y0 = foldl (\x y -> x + fst y * snd y) y0 $ zip (map head table) xDifference
  where xDifference = reverse $ init $ foldl (\x y -> (x0 - y) * head x : x) [1] xs
