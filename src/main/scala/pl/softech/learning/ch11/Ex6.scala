package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._

import scala.util.Try

object Ex6 {

  trait MonadCombinators {

    def filterM[A, F[_] : Monad](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
      case Nil => Monad[F].pure(List.empty[A])
      case h :: t => for {
        p <- f(h)
        rest <- filterM(t)(f)
      } yield if (p) h :: rest else rest
    }

  }

  def main(args: Array[String]): Unit = {

    def parse(x: String): Option[Int] = Try(x.toInt).toOption

    def isEven(x: Int): Boolean = x % 2 == 0

    def isEvenF(x: String): Option[Boolean] = parse(x).map(isEven)

    filterM(List("1", "2", "3", "4"))(isEvenF) === Some(List("2", "4"))

    filterM(List("1", "a"))(isEvenF) === None

  }

}
