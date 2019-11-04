package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._

import scala.util.Try

object Ex13 {

  // in terms of join & map

  def flatMap[A, B, F[_] : Monad](fa: F[A])(f: A => F[B]): F[B] = Monad[F].join(fa.map(f))

  def compose[A, B, C, F[_] : Monad](f: A => F[B], g: B => F[C]): A => F[C] = a =>
    Monad[F].join(f(a).map(g))

  def main(args: Array[String]): Unit = {

    def parse(x: String): Option[Int] = Try(x.toInt).toOption

    def liftF[A, B, F[_] : Monad](f: A => B): F[A] => F[B] = fa => fa.map(f)

    def inc(x: Int): Int = x + 1

    def toList[A]: Option[A] => List[A] = _.toList

    flatMap(List("1", "2", "3"))(parse _ andThen liftF(inc) _ andThen toList) === List(2, 3, 4)

    flatMap(Option("a"))(parse) === None

    val ff = compose(parse _ andThen toList, inc _ andThen ((x: Int) => List.fill(3)(x)))

    ff("1") === List(2, 2, 2)

  }

}
