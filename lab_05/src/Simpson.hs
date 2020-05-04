module Simpson (
    simpson,
    limits
) where

type N = Int
type M = Int

data Limits = Limits { a :: Double,
                       b :: Double,
                       c :: Double,
                       d :: Double
                     } deriving (Show)

limits :: Limits
limits = Limits 0.0 (pi / 2) 0.0 (pi / 2)

simpson :: N -> M -> Limits -> Double
simpson n m limits = (h * sum_of) / 3
    where h = (b limits - a limits) / (fromIntegral n)
          sum_of = foldr1 (\i acc -> acc + gauss i + 4 * gauss i + 1 + gauss i + 2) [0..n - 2 - 1]
