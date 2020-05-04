module Gauss (
    --gauss,
    getCoeffs
) where

import Math.Polynomial.Legendre

type M = Int
type Index = Int
type Tau = Double

type Matrix = [[Double]]
type Coeffs = [Double]
type Roots = [Double]

data Limits = Limits { a :: Double,
                       b :: Double,
                       c :: Double,
                       d :: Double
                     } deriving (Show)

limits :: Limits
limits = Limits 0.0 (pi / 2) 0.0 (pi / 2)

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

eps :: Double
eps = 0.001

f :: Double -> Double -> Tau -> Double
f phi theta tau = (1 - ((exp 1) * (-tau * 2 * (cos phi)) /
    (1 - (sin phi) * (sin phi) * (cos theta) * (cos theta)))) * (cos phi) * (sin phi)

sumDouble :: Double -> Double -> Double
sumDouble a b = (a + b)

diffDouble :: Double -> Double -> Double
diffDouble a b =  (b - a)

value :: Double -> Double -> Double -> Double
value a b root = (sumDouble a  b) / 2 + root * (diffDouble a b) / 2

gauss :: M -> Index -> Tau -> Limits -> Double
gauss m ind tau limits = h *  sum_of
    where roots = legendreRoots m eps
          a_ = a limits
          b_ = b limits
          c_ = c limits
          d_ = d limits
          h = (d_ - c_) / 2
          a_coeffs = getCoeffs roots
          --sum_of = sum a_coeffs
          sum_of = foldr (\x acc -> acc + (snd x * f (value a_ b_ $ fst x) (value c_ d_ $ fst x) tau)) 0 $ zip roots a_coeffs
