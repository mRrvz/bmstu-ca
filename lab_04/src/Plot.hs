module Plot (
    plotApproximation
) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: (Double -> Double) -> [Double] -> [(Double,Double)]
signal f xs = [ (x, f x) | x <- xs ]

plotApproximation f pts = toFile def "res.png" $ do
    layout_title .= "rms approximation"
    setColors [opaque blue, opaque red]
    plot (line "polynom" [signal f [-1,(-0.9)..8]])
    plot (points "points" pts)
