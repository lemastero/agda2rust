mod test.Hello {
enum Bool{
[test.Hello.Bool.false, test.Hello.Bool.true]
}


fn not() {
case 0 of
  test.Hello.Bool.false -> done[] test.Hello.Bool.true
  test.Hello.Bool.true -> done[] test.Hello.Bool.false
}

}
