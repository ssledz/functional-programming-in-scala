package pl.softech.learning.ch13

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.Free.{FlatMap, Return, Suspend}

import scala.annotation.tailrec

object Ex3 {

  @tailrec
  private def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def run[F[_], A](free: Free[F, A])(implicit F: Monad[F]): F[A] = step(free) match {
    case Return(a) => F.pure(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  def main(args: Array[String]): Unit = {
    
  }

}
