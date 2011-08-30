{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Record where

#ifdef PROMOTION
#else
data True
data False
#endif

#ifdef PROMOTION
type family Or (b1 :: Bool) (b2 :: Bool) :: Bool
#else
type family Or b1 b2
#endif
type instance Or True b2 = True
type instance Or False b2 = b2

#ifdef PROMOTION
data Nat = Zero | Succ Nat
#else
data Zero
data Succ n
#endif

#ifdef PROMOTION
type family EqNat (m :: Nat) (n :: Nat) :: Bool
#else
type family EqNat m n
#endif
type instance EqNat Zero Zero = True
type instance EqNat (Succ m) (Succ n) = EqNat m n
type instance EqNat Zero (Succ n) = False
type instance EqNat (Succ m) Zero = False

data RepNat n where
  SZero :: RepNat Zero
  SSucc :: RepNat n -> RepNat (Succ n)

#ifdef PROMOTION
data PairType a b = Pair a b
#else
data Pair x y
#endif

#ifdef PROMOTION
data List a = Nil | Cons a (List a)
#else
data Nil
data Cons x xs
#endif

data Record r where
  RNil :: Record Nil
  RCons :: (Show a, InDomain n r ~ False) =>
           RepNat n -> a -> Record r -> Record (Cons (Pair n a) r)

#ifdef PROMOTION
type family InDomain (n :: Nat) (r :: List (PairType Nat *)) :: Bool
#else
type family InDomain n r
#endif
type instance InDomain n Nil = False
type instance InDomain n (Cons (Pair m a) r) = EqNat n m `Or` InDomain n r

#ifdef PROMOTION
type family Meta (r :: List (PairType Nat *)) :: List (PairType Nat *)
#else
type family Meta r
#endif
type instance Meta Nil = Nil
type instance Meta (Cons (Pair n a) r) = Cons (Pair n (String, a -> String)) (Meta r)

mkTable :: Record r -> Record (Meta r) -> String
mkTable RNil RNil = ""
mkTable (RCons _ x r) (RCons _ (th, td) mr) =
  concat ["<tr> <th>",th,"</th> <td>",td x,"</td> </tr>"]
  ++ mkTable r mr
-- Impossible patterns:
mkTable RNil (RCons _ _ _) = undefined
mkTable (RCons _ _ _) RNil = undefined

