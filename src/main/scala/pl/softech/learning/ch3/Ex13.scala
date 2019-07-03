package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex13 {

  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = l.revers.foldRight(z)(f)

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val rev = l.foldLeft[List[A]](Nil)(Cons(_, _))
    rev.foldLeft(z)(f)
  }

  def main(args: Array[String]): Unit = {
    println(foldRight(List(1, 2, 3, 4), 0)(_ + _))
  }

}
