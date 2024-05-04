mod hello {

enum Rgb {
  Red, Green, Blue
}

enum WeekDay {
  Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday
}

pub fn id_rgb(x: Rgb) -> Rgb {
  return x;
}

pub struct ThePair {
  pairSnd: WeekDay,
  pairFst: Rgb
}

pub struct Foo {
  foo: ThePair
}


}
