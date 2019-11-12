package pl.softech.learning.ch13

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.TailRec.FlatMap

import scala.annotation.tailrec

sealed trait TailRec[A] {

  self =>

  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(self, f)

  def map[B](f: A => B): TailRec[B] = flatMap(a => TailRec.pure(f(a)))

}

object TailRec {

  def pure[A](a: A): TailRec[A] = Return(a)

  def suspend[A](fa: => TailRec[A]): TailRec[A] =
    Suspend(() => ()).flatMap(_ => fa)

  @tailrec
  def run[A](fa: TailRec[A]): A = fa match {
    case Return(a) => a
    case Suspend(r) => r()
    //    case FlatMap(fa, f) => run(f(run(fa)))
    case FlatMap(fb, f: (Any => TailRec[A])) => fb match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(fc, g: (Any => TailRec[_])) => run(fc.flatMap(a => g(a).flatMap(f)))
    }
  }

  implicit val tailRecMonadInstance: Monad[TailRec] = new Monad[TailRec] {

    def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = fa.flatMap(f)

    def pure[A](a: A): TailRec[A] = TailRec.pure(a)

    override def compose[A, B, C](f: A => TailRec[B], g: B => TailRec[C]): A => TailRec[C] =
      a => TailRec.suspend(flatMap(f(a))(g)) // coroutine
  }

  case class Return[A](a: A) extends TailRec[A]

  case class Suspend[A](resume: () => A) extends TailRec[A]

  case class FlatMap[A, B](fa: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

}