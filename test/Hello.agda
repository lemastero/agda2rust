module test.Hello where

-- Type with two inhabitants
data Bool : Set where
  false true : Bool
{-# COMPILE AGDA2RUST Bool #-}

{- Logical connective not - negation -}
not : Bool -> Bool
not true  = false
not false = true
{-# COMPILE AGDA2RUST not #-}
