mod test.Test {


enum Exp {
  Plus, Int, Var
}



pub fn eval({a : Set} (env : @0 -> Agda.Builtin.Nat.Nat) (a : test.Test.Exp @1)
(b : test.Test.Exp @2) |-
  {a = _@3} env@2 (test.Test.Exp.Plus a@1 b@0) =
    Agda.Builtin.Nat._+_
      (test.Test.eval {@3} @2 @1) (test.Test.eval {@3} @2 @0) :
      Agda.Builtin.Nat.Nat) {
  case 2 of
  test.Test.Exp.Plus ->
    done[{_}, env, a, b]
      Agda.Builtin.Nat._+_
        (test.Test.eval {@3} @2 @1) (test.Test.eval {@3} @2 @0)
  test.Test.Exp.Int -> done[{_}, env, n] @0
  test.Test.Exp.Var -> done[{_}, env, x] @1 @0
}

pub fn sum(|- Agda.Builtin.List.List.[] = 0 : Agda.Builtin.Nat.Nat) {
  case 0 of
  Agda.Builtin.List.List.[] -> done[] 0
  Agda.Builtin.List.List._∷_ ->
    done[x, xs] Agda.Builtin.Nat._+_ @1 (test.Test.sum @0)
}

pub fn _++_({a : Set} (ys : Agda.Builtin.List.List {lzero} @0) |-
  {a = _@1} Agda.Builtin.List.List.[] ys@0 =
    @0 : Agda.Builtin.List.List {lzero} @1) {
  case 1 of
  Agda.Builtin.List.List.[] -> done[{_}, ys] @0
  Agda.Builtin.List.List._∷_ ->
    done[{_}, x, xs, ys]
      Agda.Builtin.List.List._∷_ @2 (test.Test._++_ {@3} @1 @0)
}

pub fn map({a : Set} {b : Set} (f : @1 -> @0) |-
  {a = _@2} {b = _@1} f@0 Agda.Builtin.List.List.[] =
    Agda.Builtin.List.List.[] : Agda.Builtin.List.List {lzero} @1) {
  case 3 of
  Agda.Builtin.List.List.[] ->
    done[{_}, {_}, f] Agda.Builtin.List.List.[]
  Agda.Builtin.List.List._∷_ ->
    done[{_}, {_}, f, x, xs]
      Agda.Builtin.List.List._∷_ (@2 @1) (test.Test.map {@4} {@3} @2 @0)
}

pub fn plus3(|-
  = test.Test.map
      {Agda.Builtin.Nat.Nat} {Agda.Builtin.Nat.Nat}
      (λ n -> Agda.Builtin.Nat._+_ @0 3) :
      Agda.Builtin.List.List {lzero} Agda.Builtin.Nat.Nat ->
        Agda.Builtin.List.List {lzero} Agda.Builtin.Nat.Nat) {
  done[]
  test.Test.map
    {Agda.Builtin.Nat.Nat} {Agda.Builtin.Nat.Nat}
    (λ n -> Agda.Builtin.Nat._+_ @0 3)
}

pub fn doubleLambda(|-
  = λ a ->
      λ b -> Agda.Builtin.Nat._+_ @1 (Agda.Builtin.Nat._*_ 2 @0) :
      Agda.Builtin.Nat.Nat ->
        Agda.Builtin.Nat.Nat -> Agda.Builtin.Nat.Nat) {
  done[]
  λ a -> λ b -> Agda.Builtin.Nat._+_ @1 (Agda.Builtin.Nat._*_ 2 @0)
}


}
