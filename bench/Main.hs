import Criterion.Main

import Numeric.Natural

import Math.Combinatorics.Exact.Binomial
import Math.Combinatorics.Exact.Factorial

main :: IO ()
main = defaultMain
    [bgroup "factorial" [bench "65535 :: Natural" $ whnf (factorial :: _ -> Natural) 65535,
                         bench "12 :: Word" $ whnf (factorial :: _ -> Word) 12],
     bgroup "binomial"  [bench "70000,10000 :: Natural" $ whnf (uncurry choose :: _ -> Natural) (70000, 10000),
                         bench "75,6 :: Word" $ whnf (uncurry choose :: _ -> Word) (75, 6)]]
