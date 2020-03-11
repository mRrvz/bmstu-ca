module Spline(
    spline
) where

import Data.Tuple.Select
import Data.List

type ValueTable = [[Double]]

findClosest :: Int -> [Double] -> Double -> Int
findClosest n xs x_value = round $ min'
  where min' = foldl (\y x -> if (abs $ snd x - x_value) < y then fst x else y) (head xs) (zip [0..(fromIntegral n)] xs)

spline :: ValueTable -> Double -> [Double]
spline table x_value = [final, ak, bk, ck, dk]
    where
        xs = map head table
        ys = map last table
        h = 0 : (map (\x -> fst x - snd x) $ zip (drop 1 xs) (xs))
        a = 0 : init h
        b = 0 : 0 : (map (\x -> -2 * (fst x + snd x)) $ zip (drop 1 h) (drop 2 h))
        d = 0 : 0 : (drop 2 h)
        k = zip (drop 1 h) (drop 2 h)
        k1 = zip3 (drop 2 ys) (drop 1 ys) ys
        f = 0 : 0 : (map (\x -> - 3 * (((sel1 (snd x) - sel2 (snd x)) / (fst $ fst x)) - ((sel2 (snd x) - sel3 (snd x)) / (snd $ fst x)))) (zip k k1))
        ksi = foldl (\y x -> y ++ [sel1 x / (sel2 x - sel3 x * last y)]) [0, 0, 0] $ zip3 (drop 2 d) (drop 2 b) (drop 2 a)
        eta = foldl (\y x -> y ++ [(sel1 x * last y + sel2 x) / (sel3 x - sel1 x * sel4 x)]) [0, 0, 0] $ zip4 (drop 2 a) (drop 2 f) (drop 2 b) (drop 2 ksi)
        c = foldl (\y x -> (sel1 x * head y + sel2 x) : y) [0, 0] $ zip (tail $ reverse $ drop 1 ksi) (tail $ reverse $ drop 1 eta)
        a_m = 0 : (init ys)
        d_m = (map (\x -> (sel1 x - sel2 x) / (3 * sel3 x)) $ zip3 c (tail $ c) (reverse $ tail h)) ++ [0]
        q1 = reverse ys
        q2 = tail $ reverse ys
        q3 = tail h
        q4 = c
        q5 = tail $ c
        b_m = (map (\x -> (sel1 x - sel2 x) / sel3 x - (sel3 x * (sel4 x + 2 * sel5 x) / 3)) $ zip5 q1 q2 q3 q4 q5) ++ [0]
        near = findClosest (length table) xs x_value
        ak = a_m !! near
        bk = ((reverse b_m) !! near) * (x_value - (xs !! (near -  1)))
        ck = ((reverse c) !! near) * ((x_value - xs !! (near - 1))) ^ 2
        dk = ((reverse d_m) !! near) * ((x_value - xs !! (near - 1))) ^ 3
        final = ak + bk + ck + dk
