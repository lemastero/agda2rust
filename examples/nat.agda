{-# OPTIONS --erasure #-}

data Nat : Set where
  Zero : Nat
  Succ : Nat → Nat

add : Nat → Nat → Nat
add n Zero = n
add n (Succ m) = Succ (add n m)

data Eq {A : Set} (@0 a : A) : @0 A → Set where
  Refl : Eq a a

cong : {A B : Set} → {a b : A} → (f : A → B) → Eq a b → Eq (f a) (f b)
cong f Refl = Refl

sym : {A : Set} → {a b : A} → Eq a b → Eq b a
sym Refl = Refl

trans : {A : Set} → {a b c : A} → Eq a b → Eq b c → Eq a c
trans Refl Refl = Refl

add-left-id : (a : Nat) → Eq (add Zero a) a
add-left-id Zero = Refl
add-left-id (Succ a) = cong Succ (add-left-id a)

succ-left-add : (a b : Nat) → Eq (add (Succ a) b) (Succ (add a b))
succ-left-add a Zero = Refl
succ-left-add a (Succ b) = cong Succ (succ-left-add a b)

add-comm : ∀ (a b : Nat) → Eq (add a b) (add b a)
add-comm a Zero = sym (add-left-id a)
add-comm a (Succ b) = trans (cong Succ (add-comm a b)) (sym (succ-left-add b a))
