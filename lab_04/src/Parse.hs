module Parse (
    parseTable,
    Table (xy, weight),
    toInt
) where

type ValueTable = [[Double]]

data Table = Table { xy :: [(Double, Double)],
                     weight :: [Double]
                   } deriving (Show)

parseTable :: [String] -> Table
parseTable string = Table xy ro
    where words' = map words string
          table = map (map (\x -> read x :: Double)) $ map init words'
          xy = map (\x -> (,) (head x) (last x)) table
          ro = map (\x -> read x :: Double) $ map last words'

toInt :: String -> Int
toInt x = read x :: Int
