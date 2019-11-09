package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch11.ApplicativeInstances._

object Ex9 {

  def main(args: Array[String]): Unit = {

    val F = Applicative[List]
    val G = F.compose[Option]

    G.pure(1) === List(Some(1))

    G.map2(G.pure(1), G.pure(2))(_ + _) === List(Some(3))

  }

}
