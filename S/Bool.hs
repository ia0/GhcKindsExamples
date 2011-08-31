{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module S.Bool where

-- IA0: this should be generated with %False and %True
data Rep :: Bool -> * where
  SFalse :: Rep False
  STrue  :: Rep True

fromRep :: Rep b -> Bool
fromRep SFalse = False
fromRep STrue  = True

class ToRep b where
  toRep :: Rep b
instance ToRep True where
  toRep = STrue
instance ToRep False where
  toRep = SFalse

instance Show (Rep b) where
  show = show . fromRep

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

-- IA0: make this kind polymorphic
type family If (cond :: Bool) (true :: *) (false :: *) :: *
type instance If True  true false = true
type instance If False true false = false

