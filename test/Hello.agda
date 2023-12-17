module Hello where

-- simple record type
data TheRgb : Set where
  Red Green Blue : TheRgb
{-# COMPILE AGDA2RUST TheRgb #-}

-- simple function
idRgb : TheRgb → TheRgb
idRgb rgbArg = rgbArg
{-# COMPILE AGDA2RUST idRgb #-}

data TheWeekDay : Set where
  Monday Tuesday Wednesday Thursday Friday Saturday Sunday : TheWeekDay
{-# COMPILE AGDA2RUST TheWeekDay #-}

asFriday : TheRgb → TheWeekDay
asFriday rgbArg = Friday -- TODO compile body
{-# COMPILE AGDA2RUST asFriday #-}

-- TODO multiple clauses
-- day-color : TheWeekDay → TheRgb
-- day-color Saturday = green
-- day-color Sunday   = blue
-- day-color _        = red
-- {-# COMPILE AGDA2RUST day-color #-}

-- TODO multiple arguments
-- ≡Days? : TheWeekDay → TheWeekDay → TheRgb
-- ≡Days? Saturday Saturday = green
-- ≡Days? Sunday Sunday = blue
-- ≡Days? _ _ = red
-- {-# COMPILE AGDA2RUST ≡Days? #-}

-- TODO polymorphic types

-- TODO Data.Bool
-- TODO if expressions, and, or

-- TODO Data.Nat
-- TODO arithmetic expressions

-- TODO Lists

-- TODO Data.String
-- TODO borrow types

-- TODO Data.Product

-- TODO Data.Sum

-- recursive functions
