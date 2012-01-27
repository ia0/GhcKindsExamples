{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module H.Either where

data HEither :: Either * * -> * where
  HLeft :: a -> HEither (Left a)
  HRight :: b -> HEither (Right b)

