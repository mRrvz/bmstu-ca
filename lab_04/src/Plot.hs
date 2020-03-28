module Plot (
    plotApproximation
) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: (Double -> Double) -> [Double] -> [(Double,Double)]
signal f xs = [ (x, f x) | x <- xs ]

plotApproximation f pts = toFile def "res.png" $ do
    layout_title .= "Среднеквадратическое приближение"
    setColors [opaque blue, opaque red]
    --plot (points "am pts" [(1.5,2.4),(32.0,41.0),(93.0,101.0)])
    plot (line "am" [signal f [-1,(0.1)..6]])
    plot (points "am points" pts)
