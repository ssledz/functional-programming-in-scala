package pl.softech.learning.ch9

object Ex6 {

  trait ParsersEx[ParseError, Parser[+_]] {
    self: Parsers[ParseError, Parser] =>

    def parseNOfA(a: Char): Parser[String] = for {
      n <- "[1-9][0-9]*".r.map(_.toInt)
      s <- slice(listOfN(n, char(a)))
    } yield s


  }

}
