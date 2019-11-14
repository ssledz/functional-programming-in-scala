package pl.softech.learning.ch13

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.Free._

object Ex2 {

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(fb, f) => fb match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(fc, g) => runTrampoline(fc.flatMap(a => g(a).flatMap(f)))
    }
  }

  type TailRec[A] = Free[Function0, A]

  def main(args: Array[String]): Unit = {

    val F = Monad[TailRec]

    def factorial0(n: Long): TailRec[Long] =
      if (n == 0) {
        F.pure(1)
      } else Free.suspend(F.map(factorial0(n - 1))(x => x * n))

    def factorial(n: BigDecimal): TailRec[BigDecimal] = Free.suspend(
      if (n == 0) {
        F.pure(1)
      } else F.map(factorial(n - 1))(x => x * n)
    )

    val fac1 = factorial0(3)
    val fac2 = factorial(1211)

    println(fac1)
    println(fac2)
    println(runTrampoline(fac1))
    println(runTrampoline(fac2))

  }

}
