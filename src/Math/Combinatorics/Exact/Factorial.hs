{-# LANGUAGE CPP #-}
----------------------------------------------------------------
--                                                    2012.02.02
-- |
-- Module      :  Math.Combinatorics.Exact.Factorial
-- Copyright   :  Copyright (c) 2011--2015 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98 + CPP
--
-- The factorial numbers (<http://oeis.org/A000142>). For negative
-- inputs, all functions return 0 (rather than throwing an exception
-- or using 'Maybe').
--
-- Notable limits:
--
-- * 12! is the largest factorial that can fit into 'Int32'.
--
-- * 20! is the largest factorial that can fit into 'Int64'.
--
-- * 170! is the largest factorial that can fit into 64-bit 'Double'.
----------------------------------------------------------------
module Math.Combinatorics.Exact.Factorial (factorial) where

-- N.B., we need a Custom cabal build-type for this to work.
#ifdef __HADDOCK__
import Data.Int  (Int32, Int64)
#endif
import Data.Bits

{-
-- from <http://www.polyomino.f2s.com/david/haskell/hs/CombinatoricsCounting.hs.txt>

fallingFactorial n k = product [n - fromInteger i | i <- [0..toInteger k - 1] ]
-- == factorial n `div` factorial (n-k)

risingFactorial n k = product [n + fromInteger i | i <- [0..toInteger k - 1] ]
-- == factorial (n+k) `div` factorial n

-- | A common under-approximation of the factorial numbers.
factorial_stirling :: (Integral a) => a -> a
{-# SPECIALIZE factorial_stirling ::
    Integer -> Integer,
    Int     -> Int,
    Int32   -> Int32,
    Int64   -> Int64
    #-}
factorial_stirling n
    | n < 0     = 0
    | otherwise = ceiling (sqrt (2 * pi * n') * (n' / exp 1) ** n')
  where
    n' :: Double
    n' = fromIntegral n
-}


----------------------------------------------------------------
{-
    n!  = 2^{n - popCount n}
        * \prod_{k \geq 1} \left(
              \prod_{n/2^k < j \leq 2*n/2^k}
                  bool 1 j (odd j)
          \right)^k
-}

-- | Exact factorial numbers. For a fast approximation see
-- @math-functions:Numeric.SpecFunctions.factorial@ instead. The
-- naive definition of the factorial numbers is:
--
-- > factorial n
-- >     | n < 0     = 0
-- >     | otherwise = product [1..n]
--
-- However, we use a fast algorithm based on the split-recursive form:
--
-- > factorial n =
-- >     2^(n - popCount n) * product [(q k)^k | forall k, k >= 1]
-- >   where
-- >     q k = product [j | forall j, n*2^(-k) < j <= n*2^(-k+1), odd j]
--
factorial :: (Integral a, Bits a) => Word -> a
factorial n
    | n < 2     = 1
    | otherwise = go (highestBitPosition n - 1) 0 0 1 1 1 1
  where
    -- lo  == n/2^(k+1)
    -- lo' == n/2^k
    -- qk  == product of odd @j@s for @k@ in [1..K]
    -- p   == q1 * q2 * ... * qK
    -- r   == (q1 ^ K) * (q2 ^ (K-1)) * ... * (qK ^ 1)
    -- s   == 2^{n - popCount n}
    -- go :: Int -> Word -> Word -> Word -> a -> a -> a -> a
    go k lo s hi j p r
        | Just a <- k `seq` lo `seq` s `seq` hi `seq` j `seq` p `seq` r `seq` Nothing = a
        | k >= 0 =                     -- TODO: why did old version use lo/=n ?
            let lo' = n `shiftR` k     -- TODO: use shiftRL#
                hi' = (lo' - 1) .|. 1
            in case (hi' - hi) `div` 2 of
                0 -> go (k - 1) lo' (s + lo) hi' j p r
                len -> let (q, j') = partialProduct len j
                           p' = p * q
                           r' = r * p'
                       in go (k - 1) lo' (s + lo) hi' j' p' r'
        --
        -- fromIntegral s /= fromIntegral n - popCount (fromIntegral n) = error "factorial_splitRecursive: bug in the computation of n - popCount n"
        | otherwise = r `shiftL` fromIntegral s

    -- | The product of odd @j@s between n/2^k and 2*n/2^k. @len@
    -- is the count of @j@ terms to multiply, where the @j@ state
    -- argument is the largest previously used term.
    -- partialProduct :: (Integral a) => Word -> a -> (a,a)
    partialProduct len j
        | half == 0 = (,) <!>  (j+2)        <!> (j+2)
        | len  == 2 = (,) <!> ((j+2)*(j+4)) <!> (j+4)
        | otherwise =
            let (qL, j' ) = partialProduct (len - half) j
                (qR, j'') = partialProduct half         j'
            in (,) <!> (qL*qR) <!> j''
      where
        half = len `quot` 2

        (<!>) = ($!) -- fix associativity

{-
floorLog2 :: (Integral a, FiniteBits a) => a -> Int
floorLog2 n
    | n <= 0    = error "floorLog2: argument must be positive"
    | otherwise = highestBitPosition n - 1
-}

highestBitPosition :: FiniteBits a => a -> Int
highestBitPosition a = finiteBitSize a - countLeadingZeros a
{-# SPECIALIZE highestBitPosition :: Word -> Int #-}


----------------------------------------------------------------
{-
factorial_primeSwing :: Int -> Integer
factorial_primeSwing n0
    | n0 < 0    = 0
    | n0 < 20   = smallFactorials `unsafeAt` n0
    | otherwise = go n0 `shiftL` (n0 - popCount n0)
  where
    go n
        | n < 2     = 1
        | otherwise = (go (n `div` 2) ^ 2) * swing n

    swing n
        | n < 33    = smallOddSwing `unsafeAt` n
        | otherwise =
            let count = 0
                rootN = floorSqrt n
                xs    = primes 3 rootN
                ys    = primes (rootN + 1) (n `div` 3)
            in
                forM_ xs $ \x -> do
                    let q = n
                    let p = 1
                    q := q `div` x
                    whileM_ (q > 0) $ do
                        when (q .&. 1 == 1) (p := p*x)
                        q := q `div` x
                    when (p > 1) $ do
                        primeList !! count := p
                        count := count+1
                forM_ ys $ \y -> do
                    when ((n `div` y) .&. 1 == 1) $ do
                        primeList !! count := y
                        count := count+1
                return
                    $ primorial (n `div` 2 + 1) n
                    * xmathProduct primeList 0 count

    -- With hsc2hs we can use #def to define these as static C-style arrays, and then use base:Foreign.Marshall.Array to access them. Instead of using array:Data.Array.Unboxed; Or we could try the Addr# trick used in Warp
    smallOddSwing :: UArray Int Int32
    smallOddSwing = listArray (0,32)
        [ 1, 1, 1, 3, 3, 15, 5, 35, 35, 315, 63, 693, 231, 3003
        , 429, 6435, 6435, 109395, 12155, 230945, 46189, 969969
        , 88179, 2028117, 676039, 16900975, 1300075, 35102025
        , 5014575, 145422675, 9694845, 300540195, 300540195 ]

    smallFactorials :: UArray Int Int64
    smallFactorials = listArray (0,20)
        [ 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800
        , 39916800, 479001600, 6227020800, 87178291200, 1307674368000
        , 20922789888000, 355687428096000, 6402373705728000
        , 121645100408832000, 2432902008176640000 ]


factorial_parallelPrimeSwing
-}
----------------------------------------------------------------
----------------------------------------------------------- fin.
