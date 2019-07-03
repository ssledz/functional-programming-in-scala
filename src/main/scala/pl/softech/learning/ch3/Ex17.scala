package pl.softech.learning.ch3

object Ex17 {

  def toString(l : List[Double]) : List[String] = l match {
    case Cons(head, tail) => Cons(head.toString, toString(tail))
    case Nil => Nil
  }

  def main(args: Array[String]): Unit = {
    println(toString(List(1d, 2d, 3d)))

  }

}
