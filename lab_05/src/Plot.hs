
import System.IO
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: (Double -> Double) -> [Double] -> [(Double,Double)]
signal f xs = [ (x, f x) | x <- xs ]

plotApproximation table = toFile def "graph.png" $ do
    layout_title .= "N = M = 5"
    setColors [opaque blue, opaque red]
    plot (line "ε (τ)" [table])
    --plot (points "points" pts)

parseTable :: [String] -> [(Double, Double)]
parseTable table = foldr (\x acc -> (read (head x) :: Double, read (last x) :: Double) : acc ) [] words'
    where words' = map words table

main :: IO ()
main = do
    table <- openFile "table.csv" ReadMode >>= hGetContents >>= return . parseTable . lines
    plotApproximation table
    print table
