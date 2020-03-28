module Approximation(
    quadraticApproximation,
    f
) where

import Gauss
import Parse

type Coeffs = [Double]
type Weights = [Double]

f :: Coeffs -> Double -> Double
f coeffs x = sum $ zipWith (*) coeffs (map (\y -> x ^ y) [0..length coeffs - 1])

mult3 :: Double -> Double -> Double -> Double
mult3 x y z = x * y * z

quadraticApproximation :: Table -> Int -> Coeffs
quadraticApproximation table n = gauss matrix
    where
        xs = map fst $ xy table
        ys = map snd $ xy table
        x_coeffs =
            map (\k -> sum $ zipWith (*) (map (^ k) xs) $ weight table) [0..n * 2 - 2]
        y_coeffs =
            map (\k -> sum $ zipWith3 mult3 (map (^ k) xs) ys $ weight table) [0..n - 1]
        matrix = zipWith (\x y -> x ++ [y]) (map (\x -> take n $ drop x x_coeffs) [0..n - 1]) y_coeffs
