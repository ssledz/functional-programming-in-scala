package pl.softech.learning.ch9

object Ex7 {

  trait ParsersExt[ParseError, Parser[+_]] {
    self: Parsers[ParseError, Parser] =>

    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      for {
        a <- p
        b <- p2
      } yield (a, b)

//    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
//      for {
//        a <- p
//        b <- p2
//      } yield f(a, b)

  }

}
