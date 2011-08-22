{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- August 2011

This file translates parts of HList 0.2.3 (2004 - Oleg Kiselyov, Ralf
Laemmel, Keean Schupke).

http://hackage.haskell.org/package/HList

 -}

module H.List where

import Prelude hiding ( Eq, map, length )

import S.Bool ( Rep(..) )
import qualified S.Bool as B
import S.Nat ( Nat(..), Rep(..) )
import qualified S.Nat as N

-- IA0_TODO: use notations
data List a = Nil | Cons a (List a)

data HList :: List * -> * where
  HNil  :: HList Nil
  HCons :: a -> HList as -> HList (Cons a as)

head :: HList (Cons a as) -> a
head (HCons x _xs) = x

tail :: HList (Cons a as) -> HList as
tail (HCons _x xs) = xs

type family Null (as :: List *) :: Bool
type instance Null Nil = True
type instance Null (Cons a as) = False

null :: HList as -> B.Rep (Null as)
null HNil = STrue
null (HCons {}) = SFalse

type family Length (as :: List *) :: Nat
type instance Length Nil = Zero
type instance Length (Cons a as) = Succ (Length as)

length :: HList as -> N.Rep (Length as)
length HNil = SZero
length (HCons _ xs) = SSucc (length xs)

type family Append (as :: List *) (bs :: List *) :: List *
type instance Append Nil bs = bs
type instance Append (Cons a as) bs = Cons a (Append as bs)

append :: HList as -> HList bs -> HList (Append as bs)
append HNil bs = bs
append (HCons a as) bs = HCons a (append as bs)

type family Reverse' (as :: List *) (bs :: List *) :: List *
type instance Reverse' Nil bs = bs
type instance Reverse' (Cons a as) bs = Reverse' as (Cons a bs)

reverse' :: HList as -> HList bs -> HList (Reverse' as bs)
reverse' HNil as = as
reverse' (HCons a as) bs = reverse' as (HCons a bs)

type family Reverse (as :: List *) :: List *
type instance Reverse as = Reverse' as Nil

reverse :: HList as -> HList (Reverse as)
reverse as = reverse' as HNil

type family NaiveReverse (as :: List *) :: List *
type instance NaiveReverse Nil = Nil
type instance NaiveReverse (Cons a as) = Append (NaiveReverse as) (Cons a Nil)

-- IA0: Can we do something else here? (like reusing Reverse)
naiveReverse :: HList as -> HList (NaiveReverse as)
naiveReverse HNil = HNil
naiveReverse (HCons a as) = naiveReverse as `append` HCons a HNil

hfoldr :: (forall a as. a -> p as -> p (Cons a as)) -> p Nil -> HList bs -> p bs
hfoldr _ z HNil = z
hfoldr k z (HCons x xs) = x `k` hfoldr k z xs
-- hfoldr cons nil = go
--   where
--     go HNil = nil
--     go (HCons x xs) = cons x (go xs)

-- IA0: We need kind polymorphism.
-- type family Eq (eq :: * -> * -> Bool) (as :: List *) (bs :: List *) :: Bool
-- type instance Eq eq Nil Nil = True
-- type instance Eq eq Nil (Cons b bs) = False
-- type instance Eq eq (Cons a as) Nil = False
-- type instance Eq eq (Cons a as) (Cons b bs) = B.And (eq a b) (Eq as bs)

-- eq :: () -> HList as -> HList bs -> B.Rep (Eq as bs)
-- eq _ HNil HNil = STrue
-- eq _ HNil (HCons {}) = SFalse
-- eq _ (HCons {}) HNil = SFalse
-- eq subEq (HCons a as) (HCons b bs) = B.and (subEq a b) (eq subEq as bs)

type family LookupAt (n :: Nat) (as :: List *) :: *
type instance LookupAt Zero (Cons a as) = a
type instance LookupAt (Succ n) (Cons a as) = LookupAt n as

lookupAt :: N.Rep n -> HList as -> Maybe (LookupAt n as)
lookupAt SZero (HCons x _) = Just x
lookupAt (SSucc n) (HCons _ xs) = lookupAt n xs
lookupAt _ HNil = Nothing

-- IA0: We cannot write this.
-- type family Member (b :: *) (as :: List *) :: Bool
-- type instance Member b Nil = False
-- type instance Member b (Cons b as) = True

-- IA0: We could do this with kind polymorphism:
-- type family Member (eq :: k -> k -> *) (b :: k) (as :: List k) :: Bool
-- type instance Member eq b Nil = False
-- type instance Member eq b (Cons a as) = Or (eq a b) (Member eq b as)

-- IA0: Implement the followings if possible:
--   mapM
--   show
--   split

-- type family Map (fs :: List *) (as :: List *) :: List *
-- type instance Map Nil Nil = Nil
-- type instance Map (Cons (a -> r) fs) (Cons a as) = Cons r (Map fs as)

-- map :: HList fs -> HList as -> HList (Map fs as)
-- map HNil HNil = HNil
-- map (HCons f fs) (HCons a as) = HCons (f a) (map fs as)
-- map _ _ = error "arguments of map should have the same size"

-- mapOut HNil HNil = Nil
-- mapOut (HCons f fs) (HCons a as) = Cons (f a) (mapOut fs as)

