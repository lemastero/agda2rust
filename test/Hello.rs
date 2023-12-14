mod test.Hello {
enum Rgb {
  red, green, blue
}



pub fn idRgb((x : test.Hello.Rgb) |- x@0 = @0 : test.Hello.Rgb) {
  done[x] @0
}


}
