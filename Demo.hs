{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Tutorial where

import Prelude( error )

-- data List a = Nil | Cons a (List a)

-- head :: List a -> a
-- head (Cons x _) = x

data Maybe a = Nothing | Just a

-- head :: List a -> Maybe a
-- head (Cons x _) = Just x
-- head Nil = Nothing

-- head :: List a -> a
-- head (Cons x _) = x
-- head Nil = error "head: Nil"

data Emptiness = Empty | NotEmpty

data List a e where
  Nil :: List a Empty
  Cons :: a -> List a e -> List a NotEmpty

-- head :: List a NotEmpty -> a
-- head (Cons x _) = x

-- head_maybe :: List a e -> Maybe a
-- head_maybe (Cons x _) = Just x
-- head_maybe Nil = Nothing

data HMaybe m where
  HNothing :: HMaybe Nothing
  HJust :: a -> HMaybe (Just a)

type family IfEmpty (e :: Emptiness) a :: Maybe *
type instance IfEmpty NotEmpty a = Just a
type instance IfEmpty Empty a = Nothing

head_maybe :: List a e -> HMaybe (IfEmpty e a)
head_maybe (Cons x _) = HJust x
head_maybe Nil = HNothing

-- data SMaybe s m where
--   SNothing :: SMaybe s Nothing
--   SJust :: s a -> SMaybe s (Just a)

-- E<Type> means exists a. <Type> a

data EList a where
  EList :: List a e -> EList a

tail :: List a NotEmpty -> EList a
tail (Cons _ xs) = EList xs

-- data Nat = Zero | Succ Nat

-- S<Type> is the singleton type predicate for <Type>
-- S<Type> <term> is the singleton type for <term>

-- data SNat n where
--   SZero :: SNat Zero
--   SSucc :: SNat n -> SNat (Succ n)

-- data ENat where
--   ENat :: SNat n -> ENat

-- l<Type> means lower
-- l<Type> :: S<Type> <term> -> <Type>

-- r<Type> means raise
-- r<Type> :: <Type> -> exists (x :: <Type>). S<Type> x

-- lNat :: SNat n -> Nat
-- lNat SZero = Zero
-- lNat (SSucc sn) = Succ (lNat sn)

-- rNat :: Nat -> ENat
-- rNat Zero = ENat SZero
-- rNat (Succ n) =
--   case rNat n of { ENat sn ->
--   ENat (SSucc sn) }

-- data ListNat = Nil | Cons Nat ListNat

-- data SListNat ns where
--   SNil :: SListNat Nil
--   SCons :: SNat n -> SListNat ns -> SListNat (Cons n ns)

-- data EListNat where
--   EListNat :: SListNat ns -> EListNat

-- lListNat :: SListNat ns -> ListNat
-- lListNat SNil = Nil
-- lListNat (SCons sn sns) = Cons (lNat sn) (lListNat sns)

-- rListNat :: ListNat -> EListNat
-- rListNat Nil = EListNat SNil
-- rListNat (Cons n ns) =
--   case rNat n of { ENat sn ->
--   case rListNat ns of { EListNat sns ->
--   EListNat (SCons sn sns) } }

-- head :: SListNat (Cons n ns) -> SNat n
-- head (SCons n _) = n

-- class NotEmpty (ns :: ListNat)
-- instance NotEmpty (Cons n ns)

-- type family Head (ns :: List) :: Nat
-- type instance Head (Cons n ns) = n

-- head :: NotEmpty ns => SListNat ns -> ENat
-- head (SCons n _) = ENat n

