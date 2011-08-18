{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module S.Bool where

-- IA0: this should be generated with %False and %True
data Rep :: Bool -> * where
  SFalse :: Rep False
  STrue  :: Rep True

fromS :: Rep b -> Bool
fromS SFalse = False
fromS STrue  = True

type family And (b1 :: Bool) (b2 :: Bool) :: Bool
type instance And False b = False
type instance And True  b = b

and :: Rep b1 -> Rep b2 -> Rep (And b1 b2)
and SFalse _b = SFalse
and STrue   b = b

type family Or (b1 :: Bool) (b2 :: Bool) :: Bool
type instance Or True  b = True
type instance Or False b = b

or :: Rep b1 -> Rep b2 -> Rep (Or b1 b2)
or STrue  _b = STrue
or SFalse  b = b

type family Not (b :: Bool) :: Bool
type instance Not False = True
type instance Not True  = False

not :: Rep b -> Rep (Not b)
not STrue  = SFalse
not SFalse = STrue

-- IA0: type family If (b :: Bool) (true :: k) (false :: k) :: k
-- IA0: type instance If True  true _     = true
-- IA0: type instance If False _    false = false

