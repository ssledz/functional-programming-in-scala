package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch11.ApplicativeInstances._


object Ex3 {

  def main(args: Array[String]): Unit = {

    val F = Applicative[Option]

    F.map3(Option(1), Option(2), Option(3))(_ + _ + _) === Some(6)

    F.map4(Option(1), Option(2), Option(3), Option(4))(_ + _ + _ + _) === Some(10)

  }

}
