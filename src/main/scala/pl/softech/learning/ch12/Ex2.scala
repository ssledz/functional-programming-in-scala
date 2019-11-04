package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch11.ApplicativeInstances._

import scala.languageFeature.higherKinds

object Ex2 {

  // in terms of pure & map2
  def ap[A, B, F[_] : Applicative](fa: F[A])(fab: F[A => B]): F[B] =
    Applicative[F].map2(fa, fab)((a, f) => f(a))

  def map[A, B, F[_] : Applicative](fa: F[A])(f: A => B): F[B] =
    Applicative[F].ap(fa)(Applicative[F].pure(f))

  def map2[A, B, C, F[_] : Applicative](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {

    val F = Applicative[F]

    val ff: F[B => C] = F.ap(fa)(F.pure(f.curried))

    F.ap(fb)(ff)

  }


  def main(args: Array[String]): Unit = {

    val inc: Int => Int = x => x + 1

    def mul2: Int => Int = x => x * 2

    ap(List(1, 2, 3))(List(inc)) === List(2, 3, 4)

    ap(List(1, 2, 3))(List(inc, mul2)) === List(2, 2, 3, 4, 4, 6)

    map(List(1, 2, 3))(_ * 3) === List(3, 6, 9)

    map2(List(2), List(3))(_ * _) === List(6)

  }

}
