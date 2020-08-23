package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process._

object Ex5 {

  trait Implicits {

    implicit class ProcessComposition[I, O](val p1: Process[I, O]) {
      def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
        case Emit(head, tail) => Emit(head, p1 |> tail)
        case Halt() => Halt()
        case Await(recv) => p1 match {
          case Emit(head, tail) => tail |> recv(Some(head))
          case Await(recv2) => Await((i: Option[I]) => recv2(i) |> p2)
          case Halt() => Halt() |> recv(None)
        }
      }
    }

  }

  def main(args: Array[String]): Unit = {

    def p[I]: Process[I, I] = drop[I](5) |> take[I](3)

    val plus1 = lift[Int, Int](_ + 1)

    def pp: Process[Int, Int] = (for {
      _ <- drop[Int](4)
      x <- plus1
    } yield x) |> take(3)

    p(Stream.from(1)).toList === List(6, 7, 8)
    
    pp(Stream.from(1)).toList === List(7, 8, 9)

    val plus2 = plus1 |> plus1

    plus1(Stream(1, 2, 3)).toList === List(2, 3, 4)

    plus2(Stream(1, 2, 3)).toList === List(3, 4, 5)

  }

}
