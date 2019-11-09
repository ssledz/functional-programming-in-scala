package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch11.ApplicativeInstances._

object Ex8 {

  def main(args: Array[String]): Unit = {

    val F = Applicative[List]
    val H = F.product[Option]

    H.pure(1) === (List(1), Some(1))

    H.map2(H.pure(1), H.pure(2))(_ + _) === (List(3), Some(3))

  }

}
