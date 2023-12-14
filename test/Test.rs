mod test.Test {


enum Exp{
[test.Test.Exp.Plus, test.Test.Exp.Int, test.Test.Exp.Var]
}



fn eval() {
case 2 of
  test.Test.Exp.Plus ->
    done[{_}, env, a, b]
      Agda.Builtin.Nat._+_
        (test.Test.eval {@3} @2 @1) (test.Test.eval {@3} @2 @0)
  test.Test.Exp.Int -> done[{_}, env, n] @0
  test.Test.Exp.Var -> done[{_}, env, x] @1 @0
}
fn sum() {
case 0 of
  Agda.Builtin.List.List.[] -> done[] 0
  Agda.Builtin.List.List._∷_ ->
    done[x, xs] Agda.Builtin.Nat._+_ @1 (test.Test.sum @0)
}
fn _++_() {
case 1 of
  Agda.Builtin.List.List.[] -> done[{_}, ys] @0
  Agda.Builtin.List.List._∷_ ->
    done[{_}, x, xs, ys]
      Agda.Builtin.List.List._∷_ @2 (test.Test._++_ {@3} @1 @0)
}
fn map() {
case 3 of
  Agda.Builtin.List.List.[] ->
    done[{_}, {_}, f] Agda.Builtin.List.List.[]
  Agda.Builtin.List.List._∷_ ->
    done[{_}, {_}, f, x, xs]
      Agda.Builtin.List.List._∷_ (@2 @1) (test.Test.map {@4} {@3} @2 @0)
}
fn plus3() {
done[]
  test.Test.map
    {Agda.Builtin.Nat.Nat} {Agda.Builtin.Nat.Nat}
    (λ n -> Agda.Builtin.Nat._+_ @0 3)
}
fn doubleLambda() {
done[]
  λ a -> λ b -> Agda.Builtin.Nat._+_ @1 (Agda.Builtin.Nat._*_ 2 @0)
}

}
