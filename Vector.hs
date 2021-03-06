{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Vector where

import Prelude ( id, (.), otherwise, Bool(..) )

import S.Nat ( Nat(..), Rep(..) )
import qualified S.Nat as N

data Vector :: * -> Nat -> * where
  VNil  :: Vector a Zero
  VCons :: a -> Vector a n -> Vector a (Succ n)

data EVector a where
  EVector :: Vector a n -> EVector a

length :: Vector a n -> N.Rep n
length VNil = SZero
length (VCons _ v) = SSucc (length v)

head :: Vector a (Succ n) -> a
head (VCons x _) = x

tail :: Vector a (Succ n) -> Vector a n
tail (VCons _ xs) = xs

replicate :: N.Rep n -> a -> Vector a n
replicate SZero _ = VNil
replicate (SSucc n) x = VCons x (replicate n x)

append :: Vector a m -> Vector a n -> Vector a (N.Plus m n)
append VNil = id
append (VCons x xs) = VCons x . append xs

toList :: Vector a n -> [a]
toList VNil = []
toList (VCons x xs) = x : toList xs

-- splitAt :: N.Rep m -> Vector a (N.Plus m n) -> (Vector a m, Vector a n)
-- splitAt SZero zs = (VNil, zs)
-- splitAt (SSucc m) (VCons x zs) = (VCons x xs, ys)
--   where (xs, ys) = splitAt m zs
-- IA0: splitAt (SSucc m) VNil = undefined  -- IA0: Pattern match(es) are non-exhaustive

filter :: (a -> Bool) -> Vector a n -> EVector a
filter _ VNil = EVector VNil
filter p (VCons x xs)
  | p x = case exs of EVector fxs -> EVector (VCons x fxs)
  | otherwise = exs
  where exs = filter p xs

