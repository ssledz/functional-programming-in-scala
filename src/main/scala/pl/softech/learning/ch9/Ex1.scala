package pl.softech.learning.ch9

object Ex1 {

  trait ParsersExt[ParseError, Parser[+_]] {
    self: Parsers[ParseError, Parser] =>

    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      product(p, p2).map(f.tupled)

  }

}
