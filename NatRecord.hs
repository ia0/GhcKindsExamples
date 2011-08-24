{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module NatRecord where

import Prelude hiding( lookup )

import qualified S.Bool as B
import S.Nat( Nat(..) )
import qualified S.Nat as N
-- import qualified H.List as L

data Record :: [(Nat, *)] -> * where
  RNil :: Record '[]
  RCons :: N.Rep n -> a -> Record r -> Record ('(n, a) ': r)

test_0 :: Record ['(Zero, Int), '(Succ Zero, Bool)]
test_0 = RCons N.SZero 21 (RCons (N.SSucc N.SZero) True RNil)

type family Lookup (n :: Nat) (r :: [(Nat, *)]) :: *
type instance Lookup n ('(m, a) ': r) = B.If (N.Eq n m) a (Lookup n r)

lookup :: N.Rep n -> Record r -> Lookup n r
lookup _ RNil = error "bad index"
lookup n (RCons m v r) =
  case N.eq n m of
    B.STrue -> v
    B.SFalse -> lookup n r

test_1 :: Int
test_1 = lookup N.SZero test_0

test_2 :: Bool
test_2 = lookup (N.SSucc N.SZero) test_0

