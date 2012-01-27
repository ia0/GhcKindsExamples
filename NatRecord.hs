{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module NatRecord where

import Prelude hiding( lookup )
import GHC.Prim( Constraint )

import qualified S.Bool as B
import S.Nat( Nat(..) )
import qualified S.Nat as N

data RList :: (* -> Constraint) -> [(Nat, *)] -> * where
  RNil :: RList c '[]
  RCons :: (c a) => N.Rep n -> a -> RList c r -> RList c ('(n, a) ': r)

newtype Record c r = R { unR :: RList c r }

instance Show (Record Show a) where
  show (R rec) = go "{" rec
    where
       go :: String -> RList Show r -> String
       go s RNil = s ++ "}"  -- only when r is empty
       go s (RCons n a RNil) = concat [s," ",show n," = ",show a," }"]
       go s (RCons n a r) = concat [s," ",show n," = ",show a,"\n"]
                            ++ go "," r

-- IA0: It is too hard to do disjointedness checking.
-- We cannot show b2 when b1 `And` b2 ~ True

type family Lookup (n :: Nat) (r :: [(Nat, *)]) :: *
type instance Lookup n ('(m, a) ': r) = B.If (N.Eq n m) a (Lookup n r)

lookup :: N.Rep n -> Record c r -> Lookup n r
lookup _ (R RNil) = error "bad index"
lookup n (R (RCons m v r)) =
  case N.eq n m of
    B.STrue -> v
    B.SFalse -> lookup n (R r)

fold :: (forall n a as. (N.Rep n, a) -> p as -> p ('(n, a) ': as)) -> p '[] -> Record c r -> p r
fold _ z (R RNil) = z
fold k z (R (RCons n x r)) = (n, x) `k` fold k z (R r)

type family Meta (r :: [(Nat, *)]) :: [(Nat, *)]
type instance Meta '[] = '[]
type instance Meta ('(n, a) ': r) = '(n, (String, a -> String)) ': Meta r

-- IA0: We cannot write mkTable :: Record (Meta r) -> Record r -> String
mkTable :: Record c1 r -> Record c2 (Meta r) -> String
mkTable (R RNil) (R RNil) = ""
mkTable (R (RCons _ x r)) (R (RCons _ (th, td) mr)) =
  concat ["<tr> <th>",th,"</th> <td>",td x,"</td> </tr>\n"]
  ++ mkTable (R r) (R mr)
mkTable (R RNil) (R (RCons _ _ _)) = undefined  -- IA0: Record bug
mkTable (R (RCons _ _ _)) (R RNil) = undefined  -- IA0: Record bug

mkTable2 :: Record c r -> Record ZeroConstraint (Meta r) -> String
mkTable2 = mkTable

class SubType a b where
  coerce :: a -> b
instance SubType (Record c1 r) (Record c2 '[]) where
  coerce _ = R RNil
instance ( Lookup n r1 ~ a, c2 a
         , SubType (Record c1 r1) (Record c2 r2)
         , N.ToRep n )
  => SubType (Record c1 r1) (Record c2 ('(n, a) ': r2)) where
  coerce r = R $ RCons n (lookup n r) $ unR $ coerce r
    where n = N.toRep

type family Project (ns :: [Nat]) (r :: [(Nat, *)]) :: [(Nat, *)]
type instance Project '[] r = '[]
type instance Project (n ': ns) r = '(n, Lookup n r) ': Project ns r

class ZeroConstraint (a :: *)
instance ZeroConstraint a

mkShowR :: RList Show r -> Record Show r
mkShowR = R

-- TESTS -------------------------------------------------------------

type LName = Zero
lName :: N.Rep LName
lName = N.SZero

type LAge = Succ Zero
lAge :: N.Rep LAge
lAge = N.SSucc N.SZero

type Test_0 = ['(LName, String), '(LAge, Int)]

test_0 :: Record Show Test_0
test_0 = mkShowR $ RCons lName "Haskell"
                 $ RCons lAge 21
                 $ RNil

test_1 :: String
test_1 = lookup lName test_0

test_2 :: Int
test_2 = lookup lAge test_0

test_3 :: Record ZeroConstraint (Meta Test_0)
test_3 = R $ RCons lName ("Name", id)
           $ RCons lAge ("Age", show)
           $ RNil

test_4 :: String
test_4 = mkTable test_0 test_3

test_5 :: Record Show '[ '(LName, String)]
test_5 = coerce test_0

test_6 :: String
test_6 = mkTable2 test_5 (coerce test_3)

test_7 :: Record Show '[ '(LAge, Int)]
test_7 = coerce test_0

test_8 :: String
test_8 = mkTable2 test_7 (coerce test_3)

test_9 :: Record Show ['(LAge, Int), '(LName, String)]
test_9 = coerce test_0

test_10 :: String
test_10 = mkTable2 test_9 (coerce test_3)

test_11 :: Record Show (Project [LAge, LName, LAge] Test_0)
test_11 = coerce test_0

test_12 :: String
test_12 = mkTable2 test_11 (coerce test_3)

