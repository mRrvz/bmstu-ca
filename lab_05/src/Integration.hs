module Integration (
    limits,
    gauss2,
    simpson2,
) where

import Math.Polynomial.Legendre

type N = Int
type M = Int
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

eps :: Double
eps = 0.001

f :: Double -> Double -> Tau -> Double
f phi theta tau = 4 / pi * ((1 - exp(-tau * ((2 * cos phi) /
    (1 - (sin phi)^2 * (cos theta)^2)))) * (cos phi) * sin phi)

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

value :: Double -> Double -> Double -> Double
value a b root = (a + b) / 2 + root * (b - a) / 2

gauss :: Double -> M -> Tau -> Limits -> Double
gauss x m tau limits = (d limits - c limits) * sum' / 2
    where roots = legendreRoots (m + 1) eps
          ys = map (value (c limits) $ d limits) roots
          sum' = foldr (\y acc -> acc + (snd y * f x (fst y) tau)) 0 $ zip ys $ getCoeffs roots

gauss2 :: Limits -> Tau -> N -> M -> Double
gauss2 limits tau n m = (b limits - a limits) * sum' / 2
    where roots = legendreRoots (n + 1) eps
          xs = map (value (c limits) (d limits)) roots
          sum' = foldr (
            \x acc ->
                acc + (snd x * (gauss (fst x) m tau limits))) 0 $ zip xs $ getCoeffs roots

simpson2 :: Limits -> Tau -> N -> M -> Double
simpson2 limits tau n m = h / 3 * sum_of
    where h = (b limits - a limits) / (fromIntegral n)
          steps = take (n `div` 2) [a limits, a limits + 2 * h..10000]
          gauss' a = gauss a m tau limits
          sum_of = foldr (
            \a acc ->
                acc + (gauss' a) + (4 * (gauss' $ a + h) + (gauss' $ a + 2 * h))) 0 steps
