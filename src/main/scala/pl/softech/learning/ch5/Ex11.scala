package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex11 {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((res, newS)) => Stream.cons(res, unfold(newS)(f))
    case None => Stream.empty[A]
  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).toList)
    println(unfold[Int, Int](0)(x => if (x < 5) Some((x + 1, x + 1)) else None).toList)
  }

}
