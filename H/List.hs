{-# LANGUAGE TypeOperators #-}
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

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

head :: HList (a ': as) -> a
head (HCons x _) = x

tail :: HList (a ': as) -> HList as
tail (HCons _ xs) = xs

type family Null (as :: [*]) :: Bool
type instance Null '[] = True
type instance Null (a ': as) = False

null :: HList as -> B.Rep (Null as)
null HNil = STrue
null (HCons {}) = SFalse

type family Length (as :: [*]) :: Nat
type instance Length '[] = Zero
type instance Length (a ': as) = Succ (Length as)

length :: HList as -> N.Rep (Length as)
length HNil = SZero
length (HCons _ xs) = SSucc (length xs)

type family Append (as :: [*]) (bs :: [*]) :: [*]
type instance Append '[] bs = bs
type instance Append (a ': as) bs = a ': (Append as bs)

append :: HList as -> HList bs -> HList (Append as bs)
append HNil bs = bs
append (HCons a as) bs = HCons a (append as bs)

type family Reverse' (as :: [*]) (bs :: [*]) :: [*]
type instance Reverse' '[] bs = bs
type instance Reverse' (a ': as) bs = Reverse' as (a ': bs)

reverse' :: HList as -> HList bs -> HList (Reverse' as bs)
reverse' HNil as = as
reverse' (HCons a as) bs = reverse' as (HCons a bs)

type family Reverse (as :: [*]) :: [*]
type instance Reverse as = Reverse' as '[]

reverse :: HList as -> HList (Reverse as)
reverse as = reverse' as HNil

type family NaiveReverse (as :: [*]) :: [*]
type instance NaiveReverse '[] = '[]
type instance NaiveReverse (a ': as) = Append (NaiveReverse as) (a ': '[])

-- IA0: Can we do something else here? (like reusing Reverse)
naiveReverse :: HList as -> HList (NaiveReverse as)
naiveReverse HNil = HNil
naiveReverse (HCons a as) = naiveReverse as `append` HCons a HNil

hfoldr :: (forall a as. a -> p as -> p (a ': as)) -> p '[] -> HList bs -> p bs
hfoldr _ z HNil = z
hfoldr k z (HCons x xs) = x `k` hfoldr k z xs
-- hfoldr cons nil = go
--   where
--     go HNil = nil
--     go (HCons x xs) = cons x (go xs)

-- IA0: We need kind polymorphism.
-- type family Eq (eq :: * -> * -> Bool) (as :: [*]) (bs :: [*]) :: Bool
-- type instance Eq eq '[] '[] = True
-- type instance Eq eq '[] (Cons b bs) = False
-- type instance Eq eq (a ': as) '[] = False
-- type instance Eq eq (a ': as) (Cons b bs) = B.And (eq a b) (Eq as bs)

-- eq :: () -> HList as -> HList bs -> B.Rep (Eq as bs)
-- eq _ HNil HNil = STrue
-- eq _ HNil (HCons {}) = SFalse
-- eq _ (HCons {}) HNil = SFalse
-- eq subEq (HCons a as) (HCons b bs) = B.and (subEq a b) (eq subEq as bs)

type family LookupAt (n :: Nat) (as :: [*]) :: *
type instance LookupAt Zero (a ': as) = a
type instance LookupAt (Succ n) (a ': as) = LookupAt n as

lookupAt :: N.Rep n -> HList as -> Maybe (LookupAt n as)
lookupAt SZero (HCons x _) = Just x
lookupAt (SSucc n) (HCons _ xs) = lookupAt n xs
lookupAt _ HNil = Nothing

-- IA0: We cannot write this.
-- type family Member (b :: *) (as :: [*]) :: Bool
-- type instance Member b '[] = False
-- type instance Member b (Cons b as) = True

-- IA0: We could do this with kind polymorphism:
-- type family Member (eq :: k -> k -> *) (b :: k) (as :: List k) :: Bool
-- type instance Member eq b '[] = False
-- type instance Member eq b (a ': as) = Or (eq a b) (Member eq b as)

-- IA0: Implement the followings if possible:
--   mapM
--   show
--   split

-- type family Map (fs :: [*]) (as :: [*]) :: [*]
-- type instance Map '[] '[] = '[]
-- type instance Map (Cons (a -> r) fs) (a ': as) = Cons r (Map fs as)

-- map :: HList fs -> HList as -> HList (Map fs as)
-- map HNil HNil = HNil
-- map (HCons f fs) (HCons a as) = HCons (f a) (map fs as)
-- map _ _ = error "arguments of map should have the same size"

-- mapOut HNil HNil = '[]
-- mapOut (HCons f fs) (HCons a as) = Cons (f a) (mapOut fs as)

