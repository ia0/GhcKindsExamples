{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Demo where

import Prelude( error )

-- data List a = Nil | Cons a (List a)

-- head :: List a -> a
-- head (Cons x _) = x

-- data Maybe a = Nothing | Just a

-- head :: List a -> Maybe a
-- head (Cons x _) = Just x
-- head Nil = Nothing

-- head :: List a -> a
-- head (Cons x _) = x
-- head Nil = error "head: Nil"

-- data Emptiness = Empty | NotEmpty

-- data List a e where
--   Nil :: List a Empty
--   Cons :: a -> List a e -> List a NotEmpty

-- head :: List a NotEmpty -> a
-- head (Cons x _) = x

-- head_maybe :: List a e -> Maybe a
-- head_maybe (Cons x _) = Just x
-- head_maybe Nil = Nothing

-- data EList a where  -- exists e. List a e
--   EList :: List a e -> EList a  -- Pack

-- class Exist f e where
--   -- Exist ::           (* -> *) -> * -> Constraint
--   -- Exist :: forall k. (k -> *) -> * -> Constraint
--   pack :: f x -> e
--   unpack :: e -> (forall x. f x -> a) -> a

-- instance Exist (List a) (EList a) where
--   -- Exist Emptiness (List a) (EList a)
--   pack = EList
--   unpack (EList x) f = f x

-- tail :: List a NotEmpty -> EList a
-- tail (Cons _ xs) = EList xs

----------------------------------------------------------------------

-- data Nat = Zero | Succ Nat

-- data SNat n where  -- singleton types for Nat
--   SZero :: SNat Zero
--   SSucc :: SNat n -> SNat (Succ n)

-- data ENat where
--   ENat :: SNat n -> ENat

-- lNat :: SNat n -> Nat  -- lower
-- lNat SZero = Zero
-- lNat (SSucc sn) = Succ (lNat sn)

-- rNat :: Nat -> ENat  -- raise
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

