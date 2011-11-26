{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module H.Either where

data HEither :: Either * * -> * where
  HLeft :: a -> HEither (Left a)
  HRight :: b -> HEither (Right b)

