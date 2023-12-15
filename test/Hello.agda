module test.Hello where

-- simple record type
data TheRgb : Set where
  red green blue : TheRgb
{-# COMPILE AGDA2RUST TheRgb #-}

data TheWeekDay : Set where
  Monday Tuesday Wednesday Thursday Friday Saturday Sunday : TheWeekDay
{-# COMPILE AGDA2RUST TheWeekDay #-}

-- simple function
idRgb : TheRgb → TheRgb
idRgb rgbArg = rgbArg
{-# COMPILE AGDA2RUST idRgb #-}
