package pl.softech.learning.ch4

import scala.annotation.tailrec

object Ex4 {

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h match {
      case None => None
      case Some(x) => sequence2(t) match {
        case None => None
        case Some(xs) => Some(x :: xs)
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def iter(xs: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = (xs, acc) match {
      case (Nil, _) => acc
      case (Some(x) :: t, Some(l)) => iter(t, Some(x :: l))
      case _ => None
    }

    iter(a, Some(Nil)).map(_.reverse)
  }

  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), Some(2))))
    println(sequence(List(Some(1), None, Some(2))))
    println(sequence(List(None, None, None)))
  }

}
