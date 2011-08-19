{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module PatternMatch where

import Prelude hiding ( splitAt )

data Z
data S n
-- data N = Z | S N

data Rep :: * -> * where
-- data Rep :: N -> * where
  RZ :: Rep Z
  RS :: Rep n -> Rep (S n)

type family Plus (m :: *) (n :: *) :: *
-- type family Plus (m :: N) (n :: N) :: N
type instance Plus Z n = n
type instance Plus (S m) n = S (Plus m n)

data Vector :: * -> * -> * where
-- data Vector :: * -> N -> * where
  VNil  :: Vector a Z
  VCons :: a -> Vector a n -> Vector a (S n)

splitAt :: Rep m -> Vector a (Plus m n) -> (Vector a m, Vector a n)
splitAt RZ zs = (VNil, zs)
splitAt (RS m) (VCons x zs) = (VCons x xs, ys)
  where (xs, ys) = splitAt m zs
-- IA0: The compiler warns about the following missing equation but fails when we add it.
-- splitAt (RS _) VNil = undefined

