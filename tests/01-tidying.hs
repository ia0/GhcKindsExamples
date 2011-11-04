{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -ddump-tc #-}

{-

The kind variables generalized in the output of -ddump-tc are not
tidied. This should be done somewhere in kindGeneralizeKinds or
zonkQuantifiedTyVars.

-}

module ShouldCompile where

data T a b = K
