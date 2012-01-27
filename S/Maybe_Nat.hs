{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module S.Maybe_Nat where

import S.Nat( Nat(..) )
import qualified S.Nat as N

data Rep :: Maybe Nat -> * where
  SNothing :: Rep Nothing
  SJust :: N.Rep n -> Rep (Just n)

fromRep :: Rep m -> Maybe Nat
fromRep SNothing = Nothing
fromRep (SJust n) = Just (N.fromRep n)

