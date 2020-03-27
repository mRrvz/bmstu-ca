module Parse (
    parseTable,
    toDouble
) where

type ValueTable = [[Double]]

parseTable :: [String] -> ValueTable
parseTable = map (map (\x -> read x :: Double)) . map words

toDouble :: String -> Double
toDouble x = read x :: Double
