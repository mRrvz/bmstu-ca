module Interpolation (
    takeApproximation
) where
import Data.List
import Data.Maybe

slice :: [(Double, Double)] -> Int -> Int -> [(Double, Double)]
slice table n pos = take n $ drop pos table

takeApproximation :: [(Double, Double)] -> Double -> Int -> [(Double, Double)]
takeApproximation table x0 n
    | (<) x0 . fst $ head table = take n table
    | (>) x0 . fst $ last table = take n $ reverse table
    | otherwise = left ++ right
        where
            index = (fromJust $ findIndex (\x -> fst x >= x0) table)
            left = slice table (round $ n / 2) index
            right = slice table (n - length left) (index - n - length left)
