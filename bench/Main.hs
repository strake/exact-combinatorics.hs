import Criterion.Main

import Numeric.Natural

import Math.Combinatorics.Exact.Factorial

main :: IO ()
main = defaultMain [bgroup "factorial" [bench "65535" $ whnf (factorial :: _ -> Natural) 65535]]
