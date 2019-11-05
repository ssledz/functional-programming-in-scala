package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch11.MonadInstances._

import scala.util.Try

object Ex5 {

  trait MonadInstances {

    implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
        case Right(a) => f(a)
        case l@Left(_) => l.asInstanceOf[Either[E, B]]
      }

      override def pure[A](a: A): Either[E, A] = Right(a)
    }

  }

  def main(args: Array[String]): Unit = {

    val F = Monad[Either[String, *]]

    def parse(s: String): Either[String, Int] =
      Try(s.toInt).toEither.left.map(_ => s"Error parsing $s")

    val add = for {
      a <- F.pure(1)
      b <- F.pure(2)
    } yield a + b

    add === F.pure(3)

    val add3Err = for {
      a <- parse("1")
      b <- parse("b")
      c <- parse("c")
    } yield a + b + c

    add3Err === Left("Error parsing b")

    F.map2(parse("1"), parse("c"))(_ + _) === Left("Error parsing c")

  }

}
