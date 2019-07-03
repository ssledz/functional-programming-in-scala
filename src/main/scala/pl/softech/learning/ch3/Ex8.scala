package pl.softech.learning.ch3

object Ex8 {

  def main(args: Array[String]): Unit = {
    println(Ex7.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  }

}
