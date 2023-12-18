data Sum (A B : Set) : Set where
  Inj1 : A → Sum A B
  Inj2 : B → Sum A B

data UnusedExample (A B : Set) : Set where
  OnlyLeft : A → UnusedExample A B
