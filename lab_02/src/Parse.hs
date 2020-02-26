module Parse (
    Table (..),
    parseTable,
    toIntTuple,
    toDoubleTuple
) where

data Table = Table {
    valueMatrix :: [[Double]],
    l :: Int,
    m :: Int
} deriving (Show)

parseTable :: [String] -> Table
parseTable stringTable = Table {valueMatrix = value, l = l, m = m}
    where
        strDigits = map words stringTable
        l = read (head $ head strDigits) :: Int
        m = read (last $ head strDigits) :: Int
        value = map (map (\x -> read x :: Double)) (tail strDigits)

toIntTuple :: [String] -> (Int, Int)
toIntTuple list = (head numbersList, last numbersList)
    where numbersList = map (\x -> read x :: Int) list

toDoubleTuple :: [String] -> (Double, Double)
toDoubleTuple list = (head numbersList, last numbersList)
    where numbersList = map (\x -> read x :: Double) list
