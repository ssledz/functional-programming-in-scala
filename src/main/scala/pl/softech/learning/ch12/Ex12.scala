package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch11.ApplicativeInstances._


object Ex12 {

  def main(args: Array[String]): Unit = {

    val F = Applicative[Option]

    F.sequenceMap(Map(1 -> Option("one"), 2 -> Option("two"))) === Some(Map(1 -> "one", 2 -> "two"))

    F.sequenceMap(Map(1 -> Option("one"), 2 -> None)) === None

  }

}
