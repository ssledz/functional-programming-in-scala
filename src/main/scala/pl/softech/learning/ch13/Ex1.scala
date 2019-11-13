package pl.softech.learning.ch13

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.Free._
import pl.softech.learning.ch7.parallelism.NonBlocking
import pl.softech.learning.ch7.parallelism.NonBlocking.Par


object Ex1 {

  type TailRec[A] = Free[Function0, A]

  type Async[A] = Free[Par, A]

  def main(args: Array[String]): Unit = {

    val F = Monad[TailRec]

    val G = Monad[Async]

    F.pure(1) === Return(1)

    G.pure(1) === Return(1)

    def inc(x: Int): Function0[Int] = () => x + 1

    def incPar(x: Int): Par[Int] = NonBlocking.unit(x + 1)

    println(F.pure(1).flatMap(x => Suspend(inc(x))))

    println(G.pure(1).flatMap(x => Suspend(incPar(x))))

  }

}
