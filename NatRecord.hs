{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module NatRecord where

import Prelude hiding( lookup )

import qualified S.Bool as B
import S.Nat( Nat(..) )
import qualified S.Nat as N

-- IA0: We could use constraint abstraction here
-- data Record :: (* -> Constraint) -> [(Nat, *)] -> * where
--   RNil :: Record c '[]
--   RCons :: (c a, InDomain n r ~ False) =>
--            N.Rep n -> a -> Record c r -> Record c ('(n, a) ': r)
data Record :: [(Nat, *)] -> * where
  RNil :: Record '[]
  RCons :: (InDomain n r ~ False) =>
           N.Rep n -> a -> Record r -> Record ('(n, a) ': r)

type family Lookup (n :: Nat) (r :: [(Nat, *)]) :: *
type instance Lookup n ('(m, a) ': r) = B.If (N.Eq n m) a (Lookup n r)

lookup :: N.Rep n -> Record r -> Lookup n r
lookup _ RNil = error "bad index"
lookup n (RCons m v r) =
  case N.eq n m of
    B.STrue -> v
    B.SFalse -> lookup n r

type family InDomain (n :: Nat) (r :: [(Nat, *)]) :: Bool
type instance InDomain n '[] = False
type instance InDomain n ('(m, a) ': r) = N.Eq n m `B.Or` InDomain n r

fold :: (forall n a as. (N.Rep n, a) -> p as -> p ('(n, a) ': as)) -> p '[] -> Record r -> p r
fold _ z RNil = z
fold k z (RCons n x r) = (n, x) `k` fold k z r

type family Meta (r :: [(Nat, *)]) :: [(Nat, *)]
type instance Meta '[] = '[]
type instance Meta ('(n, a) ': r) = '(n, (String, a -> String)) ': Meta r

-- IA0: We cannot write mkTable :: Record (Meta r) -> Record r -> String
mkTable :: Record r -> Record (Meta r) -> String
mkTable RNil RNil = ""
mkTable (RCons _ x r) (RCons _ (th, td) mr) =
  concat ["<tr> <th>",th,"</th> <td>",td x,"</td> </tr>"]
  ++ mkTable r mr
mkTable RNil (RCons _ _ _) = undefined
mkTable (RCons _ _ _) RNil = undefined

type Test_0 = ['(Zero, String), '(Succ Zero, Int)]

test_0 :: Record Test_0
test_0 = RCons N.SZero "Haskell" $
         RCons (N.SSucc N.SZero) 21 $
         RNil

test_1 :: String
test_1 = lookup N.SZero test_0

test_2 :: Int
test_2 = lookup (N.SSucc N.SZero) test_0

test_3 :: Record (Meta Test_0)
test_3 = RCons N.SZero ("Name", id) $
         RCons (N.SSucc N.SZero) ("Age", show) $
         RNil

test_4 :: String
test_4 = mkTable test_0 test_3

