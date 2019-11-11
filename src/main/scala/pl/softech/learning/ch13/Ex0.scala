package pl.softech.learning.ch13

import pl.softech.learning.Assertion._

object Ex0 {

  def main(args: Array[String]): Unit = {

    def f[A]: A => TailRec[A] = a => TailRec.pure(a)

    val ff = List.fill(100000)(f[Int]).foldLeft(f[Int]) { (acc, a) =>
      x => TailRec.suspend(acc(x).flatMap(a))
    }

    println(ff(1))

    TailRec.run(ff(1)) === 1

  }

}
