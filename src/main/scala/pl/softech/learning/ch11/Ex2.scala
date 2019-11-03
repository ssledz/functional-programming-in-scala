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

    val one = Monad[LogState].pure(1)

    one.runA(List.empty) === 1

    val addOne = Monad[LogState].flatMap(one) { a =>
      State { log =>
        (a + 1, s"$a + 1" :: log)
      }
    }

    val (res, log) = addOne.run(List.empty)

    res === 2
    log === List("1 + 1")

  }


}
