{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Step2_Core where

-- IA0: We maybe need to differentiate promoted type constructors from
-- usual type constructors.
-- Add PromotedTypeTyCon again?

data Nat = Zero | Succ Nat

data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

data SMaybeNat :: Maybe Nat -> * where
  SNothingNat :: SMaybeNat Nothing
  SJustNat :: SNat n -> SMaybeNat (Just n)

