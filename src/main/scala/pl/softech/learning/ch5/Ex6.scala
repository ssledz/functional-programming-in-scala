package pl.softech.learning.ch5

object Ex6 {

  def headOption[A](s: Stream[A]): Option[A] = s.foldRight[Option[A]](None){
    (a, acc) => Some(a)
  }

  trait Implicits {

    implicit class Ex6Opts[A](s: Stream[A]) {
      def headOption: Option[A] = Ex6.headOption(s)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).headOption)
    println(Stream().headOption)
    println(Empty.headOption)
  }

}
