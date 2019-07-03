package pl.softech.learning.ch4

object Ex7 {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h) match {
      case l@Left(_) => l.asInstanceOf[Either[E, List[B]]]
      case Right(x) => traverse(t)(f) match {
        case e@Left(_) => e
        case Right(xs) => Right(x :: xs)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(sequence(List()))
    println(sequence(List(Right(1), Right(2))))
    println(sequence(List(Right(1), Left("error"))))
  }

}
