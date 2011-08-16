{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module HList where

import qualified SBool as B
import SNat ( Nat(..) )
import qualified SNat as N

-- IA0: use builtins
data List a = Nil | Cons a (List a)

data HList :: List * -> * where
  HNil  :: HList Nil
  HCons :: a -> HList as -> HList (Cons a as)

hhead :: HList (Cons a as) -> a
hhead (HCons x _xs) = x

htail :: HList (Cons a as) -> HList as
htail (HCons _x xs) = xs

type family HNull (as :: List *) :: Bool
type instance HNull Nil = True
type instance HNull (Cons a as) = False

hnull :: HList as -> B.Rep (HNull as)
hnull HNil = B.STrue
hnull (HCons _x _xs) = B.SFalse

type family HLength (as :: List *) :: Nat
type instance HLength Nil = Zero
type instance HLength (Cons a as) = Succ (HLength as)

hlength :: HList as -> N.Rep (HLength as)
hlength HNil = N.SZero
hlength (HCons _x xs) = N.SSucc (hlength xs)

-- TODO: Do we need a proxy for p?
hfoldr :: (forall a as. a -> p as -> p (Cons a as))
       -> p Nil -> HList bs -> p bs
hfoldr _k z HNil = z
hfoldr k z (HCons x xs) = x `k` hfoldr k z xs

type family HLookupAt (n :: Nat) (as :: List *) :: *
type instance HLookupAt Zero (Cons a as) = a
type instance HLookupAt (Succ n) (Cons a as) = HLookupAt n as

lookupAt :: N.Rep n -> HList as -> Maybe (HLookupAt n as)
lookupAt N.SZero (HCons x _xs) = Just x
lookupAt (N.SSucc n) (HCons _x xs) = lookupAt n xs
lookupAt _n HNil = Nothing

