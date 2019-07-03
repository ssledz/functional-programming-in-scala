package pl.softech.learning.ch3

object Ex16 {

  def addOne(l: List[Int]): List[Int] = l match {
    case Cons(head, tail) => Cons(head + 1, addOne(tail))
    case Nil => Nil
  }

  def main(args: Array[String]): Unit = {
    println(addOne(List(1,2,3,4)))
  }

}
