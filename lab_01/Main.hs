import Interpolation
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
    (x:n:findType) <- getArgs
    let table = initialConditions $ head findType

    putStr "Результат вычислений: "
    case head findType of
      "bisection" -> print $ bisection table (read n)
      _ -> print $ newtonPolynomial table (read x) (read n)
