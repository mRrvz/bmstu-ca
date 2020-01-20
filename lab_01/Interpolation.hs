module Interpolation (
    takeApproximation,
    createTable,
    interpolation,
) where

import Data.List
import Data.Maybe

slice :: [(Double, Double)] -> Int -> Int -> [(Double, Double)]
slice table n pos = take n $ drop pos table

takeApproximation :: [(Double, Double)] -> Double -> Int -> [(Double, Double)]
takeApproximation table x0 n
    | (<=) x0 . fst $ head table = take n table
    | (>=) x0 . fst $ last table = reverse $ take n $ reverse table
    | otherwise = left ++ right
        where
            indexL = fromJust $ findIndex (\x -> fst x >= x0) table
            left = slice table (n `div` 2 ) (indexL - n `div` 2)
            indexR = fromJust $ findIndex (== last left) table
            right = slice table (n - length left) (indexR + 1)

createTable :: [Double] -> [Double] -> Int -> [[Double]]
createTable _ (_:[]) _ = []
createTable xs ys step =  (sepDifference xs ys step) : createTable xs (sepDifference xs ys step) (step + 1)
    where
        sepDifference _ (_:[]) _ = []
        sepDifference xs ys step = (((ys !! 1) - (ys !! 0)) / ((xs !! (1 + step)) - (xs !! 0))) : sepDifference (tail xs) (tail ys) step

interpolation :: [Double] -> [[Double]] -> Double -> Double
interpolation xs [] x = 0
interpolation xs table x = ((head $ head table) + interpolation (tail xs) (tail table) x) * (x - head xs)
