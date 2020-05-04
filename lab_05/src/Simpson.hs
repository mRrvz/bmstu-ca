module Simpson (
    simpson,
    limits
) where

import Gauss

type N = Int
type M = Int

simpson :: N -> M -> Limits -> Double
simpson n m limits = (h * sum_of) / 3
    where h = (b limits - a limits) / (fromIntegral n)
          sum_of = foldr1 (\i acc -> acc + gauss i + 4 * gauss i + 1 + gauss i + 2) [0..n - 2 - 1]
