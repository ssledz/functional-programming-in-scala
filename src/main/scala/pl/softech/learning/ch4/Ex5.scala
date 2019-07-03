package pl.softech.learning.ch4

import pl.softech.learning.ch4.Option.Try

object Ex5 {

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) match {
      case None => None
      case Some(x) => traverse(t)(f) match {
        case None => None
        case Some(xs) => Some(x :: xs)
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def main(args: Array[String]): Unit = {
    println(traverse(List("1", "2"))(i => Try(i.toInt)))
    println(traverse(List("1.0", "2"))(i => Try(i.toInt)))
    println(traverse(List("1", "2.0"))(i => Try(i.toInt)))
  }

}
