{-# LANGUAGE TypeFamilies,TypeOperators,GADTs,DataKinds,UndecidableInstances,PolyKinds #-}

module Map where

-- Takes f and [a,b,..,z] and returns [f a,f b,..,f z]
type family Map (f :: * -> *) (as :: [*]) :: [*]
type instance Map f '[] = '[]
type instance Map f (a ': as) = f a ': Map f as

-- Takes [a,b,..,z] and builds a -> b -> .. -> z
type family Arr (as :: [*])
type instance Arr '[] = ()
type instance Arr (a ': '[]) = a
type instance Arr (a ': (b ': bs)) = a -> Arr (b ': bs)

-- Takes [a,b,..,z] and builds (a -> b -> .. -> z) -> [a] -> [b] -> .. -> [z]
type family Mapn (as :: [*])
type instance Mapn as = Arr as -> Arr (Map [] as)

-- Represents types at term-level in a GADT-style
data TypeRep a where
  SInt :: TypeRep Int
  SBool :: TypeRep Bool
  SPair :: TypeRep a -> TypeRep b -> TypeRep (a, b)

-- Kind-indexed type representation
-- Rep :: forall k. k -> *
type family Rep a
type instance Rep a = TypeRep a
type instance Rep as = SList as
type instance Rep '(a, b) = (Rep a, Rep b)

-- Singleton type to represent lists of types at the term-level
data SList (as :: [*]) where
  SNil :: SList '[]
  SCons :: Rep a -> SList as -> SList (a ': as)

-- Takes the representation of a list of types [a,b,..,z] and builds
-- the generic map function of type
-- (a -> b -> .. -> z) -> [a] -> [b] -> .. -> [z]
agmap :: Rep as -> Mapn as
-- We decrease on the length of the list
agmap SNil () = ()  -- not useful
agmap (SCons a SNil) x = repeat x  -- a -> [a]
agmap (SCons a (SCons b SNil)) f = \xs -> map f xs  -- (a -> b) -> [a] -> [b]
agmap (SCons a (SCons b (SCons c cs))) f =
  -- Here we build (a -> b -> ..) -> [a] -> [b] -> ..  (list of length n)
  -- using ((a, b) -> ..) -> [(a, b)] -> ..  (list of length (n - 1))
  \xs ys -> agmap (SCons (SPair a b) (SCons c cs)) (\(x, y) -> f x y) (zip xs ys)

-- Example
map_int_int_bool = agmap (SCons SInt (SCons SInt (SCons SBool SNil)))
