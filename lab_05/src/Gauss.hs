module Gauss (
    --gauss,
    getCoeffs
) where

--import Simpson
import Math.Polynomial.Legendre
import Debug.Trace

type M = Int
type Index = Int
type Tau = Double

type Matrix = [[Double]]
type Coeffs = [Double]
type Roots = [Double]

subtractRow :: [Double] -> [Double] -> [Double]
subtractRow subRow row = map (\x -> fst x - snd x * (head row / head subRow)) $ zip row subRow

triangulation :: Matrix -> Matrix
triangulation matrix
    | length matrix == 0 = matrix
    | otherwise = head matrix :
        triangulation (map tail (map (subtractRow $ head matrix) $ tail matrix))

gaussSLE :: Matrix -> Coeffs
gaussSLE = coeffs . reverse . triangulation
    where coeffs = foldl (\x y -> (last y - (sum $ zipWith (*) (init $ tail y) x)) / (head y) : x) []

getKi :: Int -> Double
getKi ind
    | ind `mod` 2 == 0 = 2 / (fromIntegral ind + 1)
    | otherwise = 0

getCoeffs :: Roots -> Coeffs
getCoeffs roots = gaussSLE $ foldr (\x acc -> ((map (^x) roots) ++ [getKi x]) : acc) [] [0..length roots - 1]

{-
eps :: Double
eps = 0.001

f :: Tau -> Double -> Double -> Double
f tau phi theta = (1 - (exp * (-tau * 2 * cos phi) / (1 - sin phi * sin phi * cos theta * cos theta))) * cos phi * sin phi

value :: Int -> Int -> Double -> Double
value a b root = (a + b) / 2 + root * (b - a) / 2

gauss :: M -> Index -> Tau -> Limits -> Double
gauss m ind tau limits = h *  sum_of
    where roots = legendreRoots m eps
          a = a limits
          b = b limits
          c = c limits
          d = d limits
          h = (d - c) / 2
          sum_of = foldr1 (\x acc -> snd x * f tau (value a b $ fst x) (value c d $ fst x)) $ zip roots a_coeffs
-}
