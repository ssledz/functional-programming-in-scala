package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process1._

object Ex7 {

  trait ProcessOps {

    def zip[I, O1, O2](p1: Process1[I, O1], p2: Process1[I, O2]): Process1[I, (O1, O2)] = (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(o1, t1), Emit(o2, t2)) => Emit((o1, o2), zip(t1, t2))
      case (Await(recv), p) => Await(i => zip(recv(i), feed(i)(p)))
      case (p, Await(recv)) => Await(i => zip(feed(i)(p), recv(i)))
    }

    def feed[I, O](input: Option[I])(p: Process1[I, O]): Process1[I, O] = p match {
      case Emit(head, tail) => Emit(head, feed(input)(tail))
      case Await(recv) => recv(input)
      case Halt() => p
    }

    implicit class ZipOps2[I, O](val p: Process1[I, O]) {
      def zip[O2](p2: Process1[I, O2]): Process1[I, (O, O2)] = Process1.zip(p, p2)

      def feed(input: Option[I]): Process1[I, O] = Process1.feed(input)(p)
    }

  }

  def mean2: Process1[Double, Double] = zip(count[Double], sum).map {
    case (c, s) => s / c
  }

  def main(args: Array[String]): Unit = {

    val plus1: Process1[Int, Int] = lift(_ + 1)
    val plus2: Process1[Int, Int] = lift(_ + 2)

    plus1.zip(plus2)(Stream(1, 2, 3)).toList === List((2, 3), (3, 4), (4, 5))

    mean2(Stream(1, 2, 3, 4, 5, 6)).toList === List(1.0, 1.5, 2.0, 2.5, 3.0, 3.5)

  }

}
