package pl.softech.learning.ch12

import pl.softech.learning.Assertion._

object Ex4 {

  def sequence[A](as: List[Stream[A]]): Stream[List[A]] =
    as.foldRight(Stream(List.empty[A])) { (fa, facc) =>
      for {
        a <- fa
        acc <- facc
      } yield a :: acc
    }

  def main(args: Array[String]): Unit = {

    lazy val ones: Stream[Int] = 1 #:: ones
    lazy val twos: Stream[Int] = 2 #:: twos

    sequence(List(Stream(1, 2), Stream(4, 5))) === Stream(List(1, 4), List(1, 5), List(2, 4), List(2, 5))

    sequence(List(ones, twos)).take(2) === Stream(List(1, 2), List(1, 2))


  }

}
