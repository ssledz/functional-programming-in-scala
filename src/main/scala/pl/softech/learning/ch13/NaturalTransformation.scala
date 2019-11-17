package pl.softech.learning.ch13

trait NaturalTransformation[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object NaturalTransformation {
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
}