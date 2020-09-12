package pl.softech.learning.ch9

/**
  * Using map2 and succeed , implement the listOfN combinator
  */
object Ex4 {

  trait ParsersExt[Parser[+_]] {
    self: Parsers[Parser] =>

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n == 0) succeed(List.empty[A])
      else map2(p, listOfN(n - 1, p))(_ :: _)

  }

}
