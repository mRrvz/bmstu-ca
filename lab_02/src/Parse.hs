module Parse (
    parseTable,
    toDouble,
    toInt,
    tuple
) where

type ValueTable = [[Double]]

parseTable :: [String] -> ValueTable
parseTable = map (map (\x -> read x :: Double)) . map words

toDouble :: String -> Double
toDouble x = read x :: Double

toInt :: String -> Int
toInt x = read x :: Int

tuple :: [String] -> (String -> a) -> (a, a)
tuple list f = (head numbersList, last numbersList)
    where numbersList = map f list
