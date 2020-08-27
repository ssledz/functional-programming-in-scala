package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process1._

object Ex4 {

  trait ProcessOps {
    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process1[I, O] = Await {
      case Some(i) => f(i, z) match {
        case (o, s2) => Emit(o, loop(s2)(f))
      }
      case None => Halt()
    }
  }

  def sum2: Process1[Double, Double] = loop(0.0) { case (i, acc) => (i + acc, i + acc) }

  def mean2: Process1[Double, Double] = loop(0.0 -> 0) { case (i, (acc, cnt)) => ((i + acc) / (cnt + 1), (i + acc, cnt + 1)) }

  def count2[I]: Process1[I, Int] = loop(0) { case (_, cnt) => (cnt + 1, cnt + 1) }

  def main(args: Array[String]): Unit = {
    count2(Stream("a", "b", "c")).toList === List(1, 2, 3)
    mean2(Stream(1, 2, 3, 4, 5, 6)).toList === List(1.0, 1.5, 2.0, 2.5, 3.0, 3.5)
    sum2(Stream(1.0, 2.0, 3.0, 4.0)).toList === List(1.0, 3.0, 6.0, 10.0)
  }

}
