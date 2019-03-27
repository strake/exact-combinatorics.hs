----------------------------------------------------------------
--                                                    2012.02.02
-- |
-- Module      :  Math.Combinatorics.Exact.Primes
-- Copyright   :  Copyright (c) 2011--2015 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
-- The prime numbers (<http://oeis.org/A000040>).
----------------------------------------------------------------
module Math.Combinatorics.Exact.Primes (primes) where

import Prelude hiding (head, tail, zipWith)
import Control.Comonad.Cofree hiding (tail)
import Control.Monad (join)
import Data.Functor.Identity
import Util.Stream

data Wheel = Wheel {-# UNPACK #-}!Word ![Word]

-- BUG: the CAF is nice for sharing, but what about when we want
-- fusion and to avoid sharing? Using Data.IntList seems to only
-- increase the overhead. I guess things aren't being memoized/freed
-- like they should...

-- | The prime numbers. Implemented with the algorithm in:
--
-- * Colin Runciman (1997)
--    /Lazy Wheel Sieves and Spirals of Primes/, Functional Pearl,
--    Journal of Functional Programming, 7(2). pp.219--225.
--    ISSN 0956-7968
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.7096>
--
primes :: Stream Word
primes = sieve wheels primes primeSquares
  where
    primeSquares = join (*) <$> primes

    wheels = Wheel 1 [1] `cons` zipWith nextSize wheels primes
      where
        nextSize (Wheel s ns) p =
            Wheel (s*p) [n' | o  <- [0,s..(p-1)*s]
                            , n  <- ns
                            , n' <- [n+o]
                            , n' `mod` p > 0 ]

    -- N.B., ps and qs must be lazy. Or else the circular program is _|_.
    sieve :: Stream Wheel -> Stream Word -> Stream Word -> Stream Word
    sieve (Cofree (Wheel s ns) (Identity ws)) ps qs =
        [ n' | o  <- s : [2*s,3*s..(head ps-1)*s]
             , n  <- ns
             , n' <- [n+o]
             , s <= 2 || noFactorIn ps qs n' ]
        `prepend` sieve ws (tail ps) (tail qs)
      where
        -- noFactorIn :: [Word] -> [Word] -> Word -> Bool
        noFactorIn (Cofree p (Identity ps)) (Cofree q (Identity qs)) x =
            q > x || x `mod` p > 0 && noFactorIn ps qs x

----------------------------------------------------------------
----------------------------------------------------------- fin.
