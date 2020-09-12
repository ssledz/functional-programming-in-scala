package pl.softech.learning.ch9

object Ex5 {

  trait ParsersExt[ParseError, Parser[+_]] {
    self: Parsers[ParseError, Parser] =>

    def lazyParser[A](p: => Parser[A]): Parser[A]


  }

}
