{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module NatRecord where

import Prelude hiding( lookup )

import qualified S.Bool as B
import S.Nat( Nat(..) )
import qualified S.Nat as N

-- IA0: We could use constraint abstraction here
-- data RList :: (* -> Constraint) -> [(Nat, *)] -> * where
--   RNil :: RList c '[]
--   RCons :: (c a) => N.Rep n -> a -> RList c r -> RList c ('(n, a) ': r)
data RList :: [(Nat, *)] -> * where
  RNil :: RList '[]
  RCons :: N.Rep n -> a -> RList r -> RList ('(n, a) ': r)

newtype Record r = R { unR :: RList r }

-- IA0: It is too hard to do validity checking.
-- We cannot show b2 when b1 `And` b2 ~ True

type family Lookup (n :: Nat) (r :: [(Nat, *)]) :: *
type instance Lookup n ('(m, a) ': r) = B.If (N.Eq n m) a (Lookup n r)

lookup :: N.Rep n -> Record r -> Lookup n r
lookup _ (R RNil) = error "bad index"
lookup n (R (RCons m v r)) =
  case N.eq n m of
    B.STrue -> v
    B.SFalse -> lookup n (R r)

fold :: (forall n a as. (N.Rep n, a) -> p as -> p ('(n, a) ': as)) -> p '[] -> Record r -> p r
fold _ z (R RNil) = z
fold k z (R (RCons n x r)) = (n, x) `k` fold k z (R r)

type family Meta (r :: [(Nat, *)]) :: [(Nat, *)]
type instance Meta '[] = '[]
type instance Meta ('(n, a) ': r) = '(n, (String, a -> String)) ': Meta r

-- IA0: We cannot write mkTable :: Record (Meta r) -> Record r -> String
mkTable :: Record r -> Record (Meta r) -> String
mkTable (R RNil) (R RNil) = ""
mkTable (R (RCons _ x r)) (R (RCons _ (th, td) mr)) =
  concat ["<tr> <th>",th,"</th> <td>",td x,"</td> </tr>\n"]
  ++ mkTable (R r) (R mr)
mkTable (R RNil) (R (RCons _ _ _)) = undefined  -- IA0: Record bug
mkTable (R (RCons _ _ _)) (R RNil) = undefined  -- IA0: Record bug

class SubType a b where
  coerce :: a -> b
instance SubType (Record r) (Record '[]) where
  coerce _ = R RNil
instance ( Lookup n r1 ~ a
         , SubType (Record r1) (Record r2)
         , N.ToRep n )
  => SubType (Record r1) (Record ('(n, a) ': r2)) where
  coerce r = R $ RCons n (lookup n r) $ unR $ coerce r
    where n = N.toRep

type family Project (ns :: [Nat]) (r :: [(Nat, *)]) :: [(Nat, *)]
type instance Project '[] r = '[]
type instance Project (n ': ns) r = '(n, Lookup n r) ': Project ns r

-- TESTS -------------------------------------------------------------

type LName = Zero
lName :: N.Rep LName
lName = N.SZero

type LAge = Succ Zero
lAge :: N.Rep LAge
lAge = N.SSucc N.SZero

type Test_0 = ['(LName, String), '(LAge, Int)]

test_0 :: Record Test_0
test_0 = R $ RCons lName "Haskell"
           $ RCons lAge 21
           $ RNil

test_1 :: String
test_1 = lookup lName test_0

test_2 :: Int
test_2 = lookup lAge test_0

test_3 :: Record (Meta Test_0)
test_3 = R $ RCons lName ("Name", id)
           $ RCons lAge ("Age", show)
           $ RNil

test_4 :: String
test_4 = mkTable test_0 test_3

test_5 :: Record '[ '(LName, String)]
test_5 = coerce test_0

test_6 :: String
test_6 = mkTable test_5 (coerce test_3)

test_7 :: Record '[ '(LAge, Int)]
test_7 = coerce test_0

test_8 :: String
test_8 = mkTable test_7 (coerce test_3)

test_9 :: Record ['(LAge, Int), '(LName, String)]
test_9 = coerce test_0

test_10 :: String
test_10 = mkTable test_9 (coerce test_3)

test_11 :: Record (Project [LAge, LAge, LName] Test_0)
test_11 = coerce test_0

test_12 :: String
test_12 = mkTable test_11 (coerce test_3)

