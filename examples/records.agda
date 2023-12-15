record Pair (A B : Set) : Set where
  field
    fst : A
    snd : B

record Foo (A : Set) : Set where
  field
    foo : Pair A A

mk-foo : {A : Set} → A → Foo A
mk-foo x = record
  { foo = record
    { fst = x
    ; snd = x } }
