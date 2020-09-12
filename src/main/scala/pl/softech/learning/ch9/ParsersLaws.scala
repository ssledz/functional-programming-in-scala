package pl.softech.learning.ch9

import pl.softech.learning.Assertion._

class ParsersLaws[Parser[+_]](P: Parsers[Parser]) {

  def runL(): Unit = {

    import P._

    for (c <- 'A' to 'z') {
      run(char(c))(c.toString) === Right(c)

      List("alaa").foreach { a =>
        run(succeed(a))(c.toString) == Right(a)
      }

    }

    List("ababa").foreach { s =>
      run(string(s))(s) === Right(s)

      List("qwerty").foreach { a =>
        run(succeed(a))(s) == Right(a)
      }

    }

    run(or(string("abra"), string("cadabra")))("abra") === Right("abra")

    run(or(string("abra"), string("cadabra")))("cadabra") === Right("cadabra")

    run(listOfN(3, "ab" | "cad"))("ababcad") === Right(List("ab", "ab", "cad"))

    run(listOfN(3, "ab" | "cad"))("cadabab") === Right(List("cad", "ab", "ab"))

    run(listOfN(3, "ab" | "cad"))("ababab") === Right(List("ab", "ab", "ab"))

    val numA: Parser[Int] = char('a').many.map(_.size)

    run(numA)("aaa") === Right(3)

    run(slice((char('a') | char('b')).many))("aaba") === Right("aaba")


  }

}
