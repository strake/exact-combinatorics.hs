import Criterion.Main

import Numeric.Natural

import Math.Combinatorics.Exact.Binomial
import Math.Combinatorics.Exact.Factorial

main :: IO ()
main = defaultMain
    [bgroup "factorial" [bench "65535" $ whnf (factorial :: _ -> Natural) 65535],
     bgroup "binomial"  [bench "70000,1000" $ whnf (uncurry choose :: _ -> Natural) (70000, 1000)]]
