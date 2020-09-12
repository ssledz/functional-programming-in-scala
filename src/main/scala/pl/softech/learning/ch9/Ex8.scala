package pl.softech.learning.ch9

object Ex8 {

  trait ParsersExt[Parser[+_]] {
    self: Parsers[Parser] =>

    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  }

}
