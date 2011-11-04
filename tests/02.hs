{-# LANGUAGE PolyKinds, GADTs #-}

module ShouldCompile where

data Eq2 a b where
  Refl :: a ~ b => Eq2 a b

convert :: Eq2 a b -> a -> b
convert Refl = id

trevnoc :: Eq2 a b -> b -> a
trevnoc Refl = id

