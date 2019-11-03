package pl.softech.learning.ch11

import pl.softech.learning.ch11.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._

import scala.util.Try

object Ex3 {

  trait MonadCombinators {
    def sequence[A, F[_] : Monad](lma: List[F[A]]): F[List[A]] = lma match {
      case Nil => Monad[F].pure(List.empty)
      case h :: t => for {
        a <- h
        as <- sequence(t)
      } yield a :: as
    }

    def traverse[A, B, F[_] : Monad](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))
  }


  def main(args: Array[String]): Unit = {

    def parse(x: String): Option[Int] = Try(x.toInt).toOption

    traverse(List("1", "2"))(parse) === Some(List(1, 2))
    traverse(List("1", "a"))(parse) === None

  }

}
