package pl.softech.learning.ch9


/**
  * Define many in terms of or , map2 , and succeed .
  */
object Ex3 {

  trait ParsersExt[Parser[+_]] {
    self: Parsers[Parser] =>

    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or succeed(List.empty)

  }

}
