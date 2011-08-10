{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Vector where

import Prelude ( id, (.) )

import qualified SNat as N

data Vector :: * -> N.Nat -> * where
  VNil  :: Vector a N.Zero
  VCons :: a -> Vector a n -> Vector a (N.Succ n)

length :: Vector a n -> N.Rep n
length VNil = N.SZero
length (VCons _ v) = N.SSucc (length v)

head :: Vector a (N.Succ n) -> a
head (VCons x _) = x

tail :: Vector a (N.Succ n) -> Vector a n
tail (VCons _ xs) = xs

replicate :: N.Rep n -> a -> Vector a n
replicate N.SZero _ = VNil
replicate (N.SSucc n) x = VCons x (replicate n x)

append :: Vector a m -> Vector a n -> Vector a (N.Plus m n)
append VNil = id
append (VCons x xs) = VCons x . append xs

toList :: Vector a n -> [a]
toList VNil = []
toList (VCons x xs) = x : toList xs

splitAt :: N.Rep m -> Vector a (N.Plus m n) -> (Vector a m, Vector a n)
splitAt N.SZero zs = (VNil, zs)
splitAt (N.SSucc m) (VCons x zs) = (VCons x xs, ys)
  where (xs, ys) = splitAt m zs
-- IA0: splitAt (N.SSucc m) VNil = undefined  -- IA0: Pattern match(es) are non-exhaustive

