mod test.Hello {
enum Rgb{
[test.Hello.Rgb.red, test.Hello.Rgb.green, test.Hello.Rgb.blue]
}



fn idRgb((x : test.Hello.Rgb) |- x@0 = @0 : test.Hello.Rgb) {
done[x] @0
}

}
