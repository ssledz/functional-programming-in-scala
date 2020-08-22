package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process.{Await, Emit, Halt, _}

object Ex1 {

  trait ProcessOps {

    def take[I](n: Int): Process[I, I] = {
      def go(acc: Int): Process[I, I] = Await[I, I] {
        case Some(x) if acc > 0 => Emit(x, go(acc - 1))
        case _ => Halt()
      }

      go(n)
    }

    def drop[I](n: Int): Process[I, I] = {
      def go(acc: Int): Process[I, I] = Await[I, I] {
        case Some(_) if acc > 0 => go(acc - 1)
        case Some(x) => Emit(x)
        case None => Halt()
      }.repeat

      go(n)
    }

    def takeWhile[I](f: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(x) if f(x) => Emit(x, takeWhile(f))
      case _ => Halt()
    }

    def dropWhile[I](f: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(x) if f(x) => dropWhile(f)
      case Some(x) => Emit(x)
      case None => Halt()
    }.repeat

  }

  def main(args: Array[String]): Unit = {

    take(5)(Stream.continually(1)).toList === List(1, 1, 1, 1, 1)

    drop(4)(Stream.from(1).take(10)).toList === List(5, 6, 7, 8, 9, 10)

    takeWhile[Int](_ < 5)(Stream.from(1)).toList === List(1, 2, 3, 4)

    dropWhile[Int](_ < 5)(Stream.from(1).take(10)).toList === List(5, 6, 7, 8, 9, 10)
  }

}
