package pl.softech.learning.ch4
import Option.Implicits._
object Ex3 {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  trait Implicits {

    implicit class Ex3Opts[A](a: Option[A]) {
      def map2[B, C](b: Option[B])(f: (A, B) => C): Option[C] = Ex3.map2(a, b)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Some(1).map2(Some(2))(_ + _))
  }

}
