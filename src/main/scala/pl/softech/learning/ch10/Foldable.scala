package pl.softech.learning.ch10

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

}

object Foldable {

  def apply[F[_]: Foldable]: Foldable[F] = implicitly[Foldable[F]]

}

object FoldableInstances extends Ex12.FoldableInstances
