module Util.Stream where

import Prelude hiding (head, tail, zipWith)
import Control.Comonad.Cofree
import Data.Foldable (toList)
import Data.Functor.Identity
import qualified Data.List as List

type Stream = Cofree Identity

cons :: a -> Stream a -> Stream a
cons a = Cofree a . Identity

prepend :: [a] -> Stream a -> Stream a
prepend as bs = foldr cons bs as

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Cofree a (Identity as)) (Cofree b (Identity bs)) = Cofree (f a b) (Identity (zipWith f as bs))

takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p = List.takeWhile p . toList

tail :: Stream a -> Stream a
tail (Cofree _ (Identity as)) = as
