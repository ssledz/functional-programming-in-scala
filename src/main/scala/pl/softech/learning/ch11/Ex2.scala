package pl.softech.learning.ch11

import pl.softech.learning.ch11.Assertion._
import pl.softech.learning.ch6.State

object Ex2 {

  trait MonadInstances {

    implicit def stateMonadInstance[S]: Monad[({type λ[A] = State[S, A]})#λ] = new Monad[({type λ[A] = State[S, A]})#λ] {
      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)

      override def pure[A](a: A): State[S, A] = State.unit(a)
    }

  }

  type LogState[A] = State[List[String], A]

  def main(args: Array[String]): Unit = {

    import MonadInstances._

    val two = Monad[LogState].pure(2)

    two.runA(List.empty) === 2

    def inc(s: LogState[Int]) = Monad[LogState].flatMap(s) { a =>
      State { log =>
        (a + 1, s"$a + 1" :: log)
      }
    }

    val (res, log) = inc(two).run(List.empty)

    res === 3
    log === List("2 + 1")

  }

}
