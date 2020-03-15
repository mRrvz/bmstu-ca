fromPairs :: (Double, Double) -> String
fromPairs pair = (show $ fst pair) ++ " " ++ (show $ snd pair) ++ "\n"

main :: IO ()
main = writeFile "table.csv" $ foldl1 (++) $ map fromPairs $ zip xs $ map f xs
    where xs = [0..1]
          f x = x * x
