package pl.softech.learning.ch4

object Ex1 {

  def main(args: Array[String]): Unit = {
    println(Some(1).map(_ + 2))
    println((None : Option[Int]).map(_ + 2))
  }

}
