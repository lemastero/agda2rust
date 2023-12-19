module test.hello where

-- simple sum type
data Rgb : Set where
  Red : Rgb
  Green : Rgb
  Blue : Rgb
{-# COMPILE AGDA2RUST Rgb #-}

data WeekDay : Set where
  Monday Tuesday Wednesday Thursday Friday Saturday Sunday : WeekDay
{-# COMPILE AGDA2RUST WeekDay #-}

-- identity function on concrete types
id_rgb : Rgb → Rgb
id_rgb x = x
{-# COMPILE AGDA2RUST id_rgb #-}

-- product types

-- record ThePair : Set where
--   field
--     pairFst : Rgb
--     pairSnd : WeekDay
-- {-# COMPILE AGDA2RUST ThePair #-}

-- record Foo (A : Set) : Set where
--   field
--     foo : Pair A A

-- TODO Data.Product as Rust tuple

-- TODO function returning constant result
-- as-friday : TheRgb → TheWeekDay
-- as-friday rgbArg = Friday
-- {-# COMPILE AGDA2RUST as-friday #-}

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

-- TODO pattern matching

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

-- TODO recursive functions
