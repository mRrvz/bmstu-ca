module Approximation(
    quadraticApproximation
) where

import Gauss

type Coeffs = [Double]
type Weights = [Double]
type Table = [[Double]]

quadraticApproximation :: Table -> Weights -> Int -> Coeffs
quadraticApproximation table weights n = gauss matrix
    where
        xs = map head table
        ys = map last table
        x_coeffs = map (\k -> sum $ zipWith (*) (map (^ k) xs) weights) [0..n * 2 - 2]
        y_coeffs = map (\k -> sum $ zipWith3 (\x y z -> x * y * z) (map (^ k) xs) ys weights) [0..n - 1]
        matrix = zipWith (\x y -> x ++ [y]) (map (\x -> take n $ drop x x_coeffs) [0..n - 1]) y_coeffs
