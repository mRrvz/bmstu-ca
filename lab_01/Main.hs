import System.IO
import Interpolation

main :: IO ()
main = do
    let xs = [1..20]
    let ys = [x * x | x <- xs]
    let table = zip xs ys
    let x0 = 6.5
    let n = 4
    print $ takeApproximation table x0 n
