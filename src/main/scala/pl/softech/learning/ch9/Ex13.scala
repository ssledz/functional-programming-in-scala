package pl.softech.learning.ch9

import pl.softech.learning.Assertion._
import pl.softech.learning.ch9.Parsers.{Location, ParseError}
import pl.softech.learning.ch9.ParsersImpl.{Failure, Success}

object Ex13 {

  def main(args: Array[String]): Unit = {

    val parser = new ParsersImpl()

    import parser._

    val s = string("abcd")
    run2(s)("abcdefg") === Success("abcd", 4)
    run2(s)("aabcdefg") == Failure(ParseError(List(Location("aabcdefg", 0) -> "Expected: abcd")))

    run2(regex("abc*".r))("aa") == Failure(ParseError(List(Location("aa", 0) -> "Expected: abc*")))
    run2(regex("abc*".r))("abccc") == Success("abccc", 5)

    run2(succeed("abc"))("abccc") == Success("abc", 0)

  }

}
