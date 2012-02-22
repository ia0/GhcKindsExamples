{-# LANGUAGE TypeFamilies,TypeOperators,GADTs,DataKinds,UndecidableInstances,PolyKinds #-}

module Map where

type family Map (f :: * -> *) (as :: [*]) :: [*]
type instance Map f '[] = '[]
type instance Map f (a ': as) = f a ': Map f as

type family Arr (as :: [*])
type instance Arr '[] = ()
type instance Arr (a ': '[]) = a
type instance Arr (a ': (b ': bs)) = a -> Arr (b ': bs)

type family Mapn (as :: [*])
type instance Mapn as = Arr as -> Arr (Map [] as)

data TypeRep a where
  SInt :: TypeRep Int
  SBool :: TypeRep Bool
  SPair :: TypeRep a -> TypeRep b -> TypeRep (a, b)

type family Rep a
type instance Rep a = TypeRep a
type instance Rep as = SList as
type instance Rep '(a, b) = (Rep a, Rep b)

data SList (as :: [*]) where
  SNil :: SList '[]
  SCons :: Rep a -> SList as -> SList (a ': as)

agmap :: Rep as -> Mapn as
agmap SNil () = ()
agmap (SCons a SNil) x = repeat x
agmap (SCons a (SCons b SNil)) f = \xs -> map f xs
agmap (SCons a (SCons b (SCons c cs))) f =
  \xs ys -> agmap (SCons (SPair a b) (SCons c cs)) (\(x, y) -> f x y) (zip xs ys)

map_int_int_bool = agmap (SCons SInt (SCons SInt (SCons SBool SNil)))
