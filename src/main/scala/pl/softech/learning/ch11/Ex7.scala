package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._

import scala.util.Try

object Ex7 {

  trait MonadCombinators {

    def compose[A, B, C, F[_] : Monad](f: A => F[B], g: B => F[C]): A => F[C] = a => for {
      b <- f(a)
      c <- g(b)
    } yield c

  }

  def main(args: Array[String]): Unit = {

    val parse: String => Option[Int] = x => Try(x.toInt).toOption

    val inc: Int => Option[Int] = x => Some(x + 1)

    val toString: Int => Option[String] = x => Some(x.toString)

    val parseAndInc = compose(parse, inc)

    parseAndInc("1") === Some(2)

    parseAndInc("a") === None

    def associativeLaw[A, B, C, D, F[_] : Monad](f: A => F[B], g: B => F[C], h: C => F[D], a: A): Unit = {
      compose(compose(f, g), h).apply(a) === compose(f, compose(g, h)).apply(a)
    }

    associativeLaw(parse, inc, toString, "1")

  }

}
