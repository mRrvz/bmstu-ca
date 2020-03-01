f :: Double -> Double -> Double
f x y = x * x  + y * y 

xs :: [Double]
xs = [1..10]

ys :: [Double]
ys = [1..5]

main :: IO ()
main = writeFile "table.csv" ("0 " ++ xborder ++ value)
    where
        xborder = foldr (\x y -> show x ++ " " ++ y) "" xs ++ "\n"
        xstring y = foldr (\x str -> show (f x y) ++ " " ++ str) "" xs ++ "\n"
        value = foldr (\y str -> show y ++ " " ++ (xstring y) ++ str) "" ys
