{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module S.Nat where

import Prelude hiding( Eq )

import qualified S.Bool as B

data Nat = Zero | Succ Nat
  deriving( Show )

-- IA0: this should be generated with %Zero and %Succ
data Rep :: Nat -> * where
  SZero :: Rep Zero
  SSucc :: Rep n -> Rep (Succ n)

fromRep :: Rep n -> Nat
fromRep SZero = Zero
fromRep (SSucc n) = Succ (fromRep n)

class ToRep n where
  toRep :: Rep n
instance ToRep Zero where
  toRep = SZero
instance (ToRep n) => ToRep (Succ n) where
  toRep = SSucc toRep

instance Show (Rep n) where
  show = show . fromRep

pred :: Rep (Succ n) -> Rep n
pred (SSucc n) = n

succ :: Rep n -> Rep (Succ n)
succ = SSucc

type family Eq (m :: Nat) (n :: Nat) :: Bool
type instance Eq  Zero     Zero    = True
type instance Eq (Succ m) (Succ n) = Eq m n
-- IA0: type instance Eq _ _ = False
type instance Eq (Succ m)  Zero    = False
type instance Eq  Zero    (Succ n) = False

eq :: Rep m -> Rep n -> B.Rep (Eq m n)
eq  SZero      SZero     = B.STrue
eq (SSucc  m) (SSucc  n) = eq m n
-- IA0: eq _ _ = SFalse
eq (SSucc _m)  SZero     = B.SFalse
eq  SZero     (SSucc _n) = B.SFalse

type family Neq (m :: Nat) (n :: Nat) :: Bool
type instance Neq m n = B.Not (Eq m n)

neq :: Rep m -> Rep n -> B.Rep (Neq m n)
neq m n = B.not (eq m n)

type family Plus (m :: Nat) (n :: Nat) :: Nat
type instance Plus Zero n = n
type instance Plus (Succ m) n = Succ (Plus m n)
-- IA0: type family SPlus m n where
-- IA0:   SPlus Zero n = n
-- IA0:   SPlus (Succ m) n = Succ (SPlus m n)

plus :: Rep m -> Rep n -> Rep (Plus m n)
plus  SZero    n = n
plus (SSucc m) n = SSucc (plus m n)

type family Le (m :: Nat) (n :: Nat) :: Bool
type instance Le Zero n = True
type instance Le (Succ m) Zero = False
type instance Le (Succ m) (Succ n) = Le m n

le :: Rep m -> Rep n -> B.Rep (Le m n)
le SZero _ = B.STrue
le (SSucc _) SZero = B.SFalse
le (SSucc m) (SSucc n) = le m n

