package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.MonadInstances._

import scala.util.Try

object Ex7 {

  def main(args: Array[String]): Unit = {

    val F = Monad[Option]

    val parse: String => Option[Int] = x => Try(x.toInt).toOption

    val inc: Int => Option[Int] = x => Some(x + 1)

    val toString: Int => Option[String] = x => Some(x.toString)

    val parseAndInc = F.compose(parse, inc)

    parseAndInc("1") === Some(2)

    parseAndInc("a") === None

    def associativeLaw[A, B, C, D, F[_] : Monad](f: A => F[B], g: B => F[C], h: C => F[D], a: A): Unit = {
      val F = Monad[F]
      F.compose(F.compose(f, g), h).apply(a) === F.compose(f, F.compose(g, h)).apply(a)
    }

    associativeLaw(parse, inc, toString, "1")

  }

}
