package pl.softech.learning.ch11

import Monad._
import MonadInstances._
import MonadSyntax._
import Assertion._

import scala.util.Try

object Ex7 {

  trait MonadCombinators {

    def compose[A,B,C, F[_] : Monad](f: A => F[B], g: B => F[C]): A => F[C] = a => for {
      b <- f(a)
      c <- g(b)
    } yield c

  }

  def main(args: Array[String]): Unit = {

    val parse : String => Option[Int] = x => Try(x.toInt).toOption

    val inc : Int => Option[Int] = x => Some(x + 1)

    val parseAndInc = compose(parse, inc)

    parseAndInc("1") === Some(2)

    parseAndInc("a") === None

  }

}
