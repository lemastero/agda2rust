module test.Hello where

data Rgb : Set where
  false Green Blue : Rgb
{-# COMPILE AGDA2RUST Rgb #-}
