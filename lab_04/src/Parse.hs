module Parse (
    parseTable,
    toInt
) where

type ValueTable = [[Double]]

parseTable :: [String] -> ValueTable
parseTable = map (map (\x -> read x :: Double)) . map words

toInt :: String -> Int
toInt x = read x :: Int
