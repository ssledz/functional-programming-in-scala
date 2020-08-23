package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process._

object Ex7 {

  trait ProcessOps {

    def zip[I, O1, O2](p1: Process[I, O1], p2: Process[I, O2]): Process[I, (Option[O1], Option[O2])] = Await[I, (Option[O1], Option[O2])] {
      case Some(x) =>
        val (o1, t1) = feed(x, p1)
        val (o2, t2) = feed(x, p2)
        Emit(o1 -> o2) ++ zip(t1, t2)
      case None => Halt()
    }

    private def feed[I, O](input: I, p: Process[I, O]): (Option[O], Process[I, O]) = {
      def go(pp: Process[I, O]): (Option[O], Process[I, O]) = pp match {
        case Emit(head, tail) => Some(head) -> tail
        case Await(recv) => go(recv(Some(input)))
        case Halt() => None -> Halt()
      }

      go(p)
    }

  }

  def mean2: Process[Double, Double] = zip(count[Double], sum).flatMap {
    case (Some(c), Some(s)) => Emit[Double, Double](s / c)
    case _ => Halt()
  }

  def main(args: Array[String]): Unit = {

    val plus1: Process[Int, Int] = lift(_ + 1)
    val plus2: Process[Int, Int] = lift(_ + 2)

    zip(plus1, plus2)(Stream(1, 2, 3)).toList === List((Some(2), Some(3)), (Some(3), Some(4)), (Some(4), Some(5)))

    mean2(Stream(1, 2, 3, 4, 5, 6)).toList === List(1.0, 1.5, 2.0, 2.5, 3.0, 3.5)

  }

}
