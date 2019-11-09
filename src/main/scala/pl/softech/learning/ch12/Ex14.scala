package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch12.TraverseInstances._

object Ex14 {

  type Id[A] = A

  implicit val idApplicativeInstance: Applicative[Id] = new Applicative[Id] {
    def pure[A](a: A): Id[A] = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }

  def map[A, B, F[_] : Traverse](fa: F[A])(f: A => B): F[B] =
    Traverse[F].traverse(fa)(a => f(a): Id[B])


  def main(args: Array[String]): Unit = {

    map(Option(1))(_ + 1) === Some(2)
    map(List(1, 2, 3))(_ + 1) === List(2, 3, 4)

  }

}
