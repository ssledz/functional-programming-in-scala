package pl.softech.learning.ch13

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.Async.FlatMap
import pl.softech.learning.ch7.parallelism.NonBlocking
import pl.softech.learning.ch7.parallelism.NonBlocking.Par

sealed trait Async[A] {

  self =>

  def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(self, f)

  def map[B](f: A => B): Async[B] = flatMap(a => Async.pure(f(a)))


}

object Async {

  private type Kleisli[A] = Any => Async[A]

  def pure[A](a: A): Async[A] = Return(a)

  def suspend[A](a: => Async[A]): Async[A] = FlatMap(pure(()), (_: Unit) => a)

  @annotation.tailrec
  private def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g: Kleisli[A]) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f: Kleisli[A]) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => NonBlocking.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f: Kleisli[A]) => x match {
      case Suspend(r) => NonBlocking.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  implicit val asyncMonadInstance: Monad[Async] = new Monad[Async] {

    def flatMap[A, B](fa: Async[A])(f: A => Async[B]): Async[B] = fa.flatMap(f)

    def pure[A](a: A): Async[A] = Async.pure(a)

    override def compose[A, B, C](f: A => Async[B], g: B => Async[C]): A => Async[C] = a => Async.suspend(flatMap(f(a))(g))
  }

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A]

  case class FlatMap[A, B](fa: Async[A], f: A => Async[B]) extends Async[B]

}
