package pl.softech.learning.ch13

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.Free.FlatMap

import scala.annotation.tailrec

sealed trait Free[F[_], A] {

  self =>

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(self, f)

  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.pure(f(a)))

}

object Free {

  def pure[F[_], A](a: A): Free[F, A] = Return(a)

  def suspend[F[_], A](fa: => Free[F, A]): Free[F, A] =
    FlatMap(pure(()), (_: Unit) => fa)

  @tailrec
  private def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def run[F[_], A](free: Free[F, A]): F[A] = step(free) match {
    case Return(a) => ???
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => ???
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  implicit def freeMonadInstance[F[_]]: Monad[Lambda[A => Free[F, A]]] = new Monad[Lambda[A => Free[F, A]]] {

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)

    override def pure[A](a: A): Free[F, A] = Free.pure(a)
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

}


