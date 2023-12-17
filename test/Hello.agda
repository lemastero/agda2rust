module test.hello where

-- simple record type
data Rgb : Set where
  Red Green Blue : Rgb
{-# COMPILE AGDA2RUST Rgb #-}

-- simple function
-- idRgb : Rgb → Rgb
-- idRgb x = x
-- {-# COMPILE AGDA2RUST idRgb #-}
