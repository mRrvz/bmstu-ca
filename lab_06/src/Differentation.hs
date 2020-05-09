module Differentation (
    leftSide,
    centerDiff,
    rungeCenter,
    alignment,
    differential2
) where

leftValue :: Double -> Double -> Double -> Double
leftValue ly y h = (ly - y) / h

leftSide :: [Double] -> Double -> [Double]
leftSide ys h = map (\y -> (fst y - snd y) / h) $ zip (tail ys) ys

centerDiff :: [Double] -> Double -> [Double]
centerDiff ys h = map (\y -> (fst y - snd y) / (2 * h)) $ zip (tail $ tail ys) ys

rungeCenter :: [Double] -> Double -> Int -> [Double]
rungeCenter ys h p = runge
    where yh = tail $ leftSide ys h
          ymh = centerDiff ys h
          runge = map (\y -> fst y + (fst y - snd y) / (2^p - 1)) $ zip yh ymh

fst' :: (Double, Double, Double) -> Double
fst' (x, _, _ ) = x

snd' :: (Double, Double, Double) -> Double
snd' (_, x, _) = x

thd' :: (Double, Double, Double) -> Double
thd' (_, _, x) = x

differential2 :: [Double] -> Double -> [Double]
differential2 ys h = map (
                    \y ->
                        (fst' y - 2 * snd' y  + thd' y) / (h * h)) $ zip3 ys (tail ys) (tail $ tail ys)

alignment :: [Double] -> [Double] -> Double -> [Double]
alignment ys xs h = a
    where ksi = map (\y -> -1 / y) ys
          phi = map (\x -> -1 / x) xs
          a = map (
            \i ->
                ((ys !! i) ^2 / (xs !! i) ^2) *
                    leftValue (ksi !! (i + 1)) (ksi !! i) (phi !! (i + 1) - phi !! i)) [0..length ys - 2]
