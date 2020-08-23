package pl.softech.learning.ch15

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch15.Process.{Await, Emit, Halt, lift}

sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this) // Restart the process if it halts on its own.
      case Await(recv) => Await {
        case None => recv(None) // Don’t repeat if terminated from source.
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

}

object Process extends Ex1.ProcessOps with Ex2.ProcessOps with Ex3.ProcessOps with Ex4.ProcessOps with Ex5.Implicits
  with Ex6.ProcessOps with Ex7.ProcessOps {

  implicit def monadInstance[I]: Monad[Process[I, *]] = new Monad[Process[I, *]] {
    def flatMap[A, B](fa: Process[I, A])(f: A => Process[I, B]): Process[I, B] = fa.flatMap(f)

    def pure[A](a: A): Process[I, A] = Emit(a)
  }

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }

    go(0.0)
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]

}
