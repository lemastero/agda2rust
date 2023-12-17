module test.Hello where

data Red : Set where
  false Green Blue : Bool
{-# COMPILE AGDA2RUST Bool #-}
