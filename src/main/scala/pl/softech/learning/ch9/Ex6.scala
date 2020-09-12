package pl.softech.learning.ch9

object Ex6 {

  def parseNOfA[Parser[+_]](a: Char)(P: Parsers[Parser]): Parser[String] = {
    import P._
    for {
      n <- "[1-9][0-9]*".r.map(_.toInt)
      s <- slice(listOfN(n, char(a)))
    } yield s
  }

}
