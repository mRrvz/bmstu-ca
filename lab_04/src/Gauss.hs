module Gauss(
    gauss
) where

type Matrix = [[Double]]
type Coeffs = [Double]

subtractRow :: [Double] -> [Double] -> [Double]
subtractRow subRow row = map (\x -> fst x - snd x * (head row / head subRow)) $ zip row subRow

triangulation :: Matrix -> Matrix
triangulation matrix
    | length matrix == 0 = matrix
    | otherwise = head matrix :
        triangulation (map tail (map (subtractRow $ head matrix) $ tail matrix))

gauss :: Matrix -> Coeffs
gauss = coeffs . reverse . triangulation
    where coeffs = foldl (\x y -> (last y - (sum $ zipWith (*) (init $ tail y) x)) / (head y) : x) []
