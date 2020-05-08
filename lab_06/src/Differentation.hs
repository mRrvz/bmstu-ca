module Differentation (
    leftSide,
    rightSide,
    centerDiff,
    rungeCenter,
    alignment,
    differential2
) where

import Debug.Trace

leftSide :: [Double] -> Double -> [Double]
leftSide ys h = foldr (\y acc -> (fst y - snd y) / h : acc) [] $ zip (tail ys) ys

--leftSideDouble :: [Double] -> Double -> [Double]
--leftSideDouble ys h = foldr (\y acc -> (fst y - snd y) / (2 * h) : acc) [] $ zip (tail ys) ys

rightSide :: [Double] -> Double -> [Double]
rightSide ys h = foldr (\y acc -> (fst y - snd y) / h : acc) [] $ zip (tail ys) ys

centerDiff :: [Double] -> Double -> [Double]
centerDiff ys h = foldr (\y acc -> (fst y - snd y) / (2 * h) : acc) [] $ zip (tail $ tail ys) ys

trace2 :: Show a => [Char] -> a -> a -> a
trace2 name x y = trace (name ++ ": " ++ show x) y

rungeCenter :: [Double] -> Double -> Int -> [Double]
rungeCenter ys h p = runge
    where yh = tail $ leftSide ys h
          ymh = centerDiff ys h
          runge = foldr (\y acc -> fst y + (fst y - snd y) / (2^p - 1) : acc) [] $ zip yh ymh

getDiff :: [Double] -> Int -> Double
getDiff a ind = a !! (ind + 1) - a !! ind

alignment :: [Double] -> [Double] -> [Double]
alignment xs ys = align
    where ksi = map log xs
          eta = map log ys
          align = foldr (
            \i acc ->
                (getDiff eta i) / (getDiff ksi i) * (ys !! i / xs !! i) : acc) [] [0..length ys - 2]

fst' :: (Double, Double, Double) -> Double
fst' (x, _, _ ) = x

snd' :: (Double, Double, Double) -> Double
snd' (_, x, _) = x

thd' :: (Double, Double, Double) -> Double
thd' (_, _, x) = x

differential2 :: [Double] -> Double -> [Double]
differential2 ys h = foldr (
    \y acc ->
        (fst' y - 2 * snd' y + thd' y) / (h * h) : acc) [] $ zip3 ys (tail ys) (tail $ tail ys)
