package pl.softech.learning.ch15

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch15.Process1.{Await, Emit, Halt, lift}

sealed trait Process1[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process1[I, O] = {
    def go(p: Process1[I, O]): Process1[I, O] = p match {
      case Halt() => go(this) // Restart the process if it halts on its own.
      case Await(recv) => Await {
        case None => recv(None) // Don’t repeat if terminated from source.
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def map[O2](f: O => O2): Process1[I, O2] = this |> lift(f)

  def ++(p: => Process1[I, O]): Process1[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process1[I, O2]): Process1[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

}

object Process1 extends Ex1.ProcessOps with Ex2.ProcessOps with Ex3.ProcessOps with Ex4.ProcessOps with Ex5.Implicits
  with Ex6.ProcessOps with Ex7.ProcessOps with Ex8.ProcessOps {

  implicit def monadInstance[I]: Monad[Process1[I, *]] = new Monad[Process1[I, *]] {
    def flatMap[A, B](fa: Process1[I, A])(f: A => Process1[I, B]): Process1[I, B] = fa.flatMap(f)

    def pure[A](a: A): Process1[I, A] = Emit(a)
  }

  def liftOne[I, O](f: I => O): Process1[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def filter[I](p: I => Boolean): Process1[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def sum: Process1[Double, Double] = {
    def go(acc: Double): Process1[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }

    go(0.0)
  }

  def lift[I, O](f: I => O): Process1[I, O] = liftOne(f).repeat

  case class Emit[I, O](head: O, tail: Process1[I, O] = Halt[I, O]()) extends Process1[I, O]

  case class Await[I, O](recv: Option[I] => Process1[I, O]) extends Process1[I, O]

  case class Halt[I, O]() extends Process1[I, O]

}
