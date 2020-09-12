package pl.softech.learning.ch9

object Ex5 {

  trait ParsersExt[Parser[+_]] {
    self: Parsers[Parser] =>

    def lazyParser[A](p: => Parser[A]): Parser[A]


  }

}
