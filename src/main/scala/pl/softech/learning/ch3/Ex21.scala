package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex21 {

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l.flatMap {
    e => if (f(e)) List(e) else Nil
  }

  def main(args: Array[String]): Unit = {
    println(filter(List(1, 2, 3, 4, 5, 6, 7))(_ % 2 == 0))
  }

}
