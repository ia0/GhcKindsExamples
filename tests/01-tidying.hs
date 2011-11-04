{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -ddump-tc #-}

{-

The kind variables generalized in the output of -ddump-tc are not
tidied. This should be done somewhere in kindGeneralizeKinds or
zonkQuantifiedTyVars.

-}

module M where

data T a b = K

{-

Current output:

[1 of 1] Compiling M                ( 01-tidying.hs, 01-tidying.o )
TYPE SIGNATURES
TYPE CONSTRUCTORS
  data T (k::BOX) (k::BOX) (a::k) (b::k)
      RecFlag NonRecursive
      = K :: forall (k::BOX) (k::BOX) (a::k) (b::k). T k k a b
      FamilyInstance: none
COERCION AXIOMS
Dependent modules: []
Dependent packages: [base, ghc-prim, integer-gmp]

==================== Typechecker ====================


Expected output:

[1 of 1] Compiling M                ( 01-tidying.hs, 01-tidying.o )
TYPE SIGNATURES
TYPE CONSTRUCTORS
  data T (k::BOX) (k0::BOX) (a::k) (b::k0)
      RecFlag NonRecursive
      = K :: forall (k::BOX) (k0::BOX) (a::k) (b::k0). T k k0 a b
      FamilyInstance: none
COERCION AXIOMS
Dependent modules: []
Dependent packages: [base, ghc-prim, integer-gmp]

==================== Typechecker ====================

-}
