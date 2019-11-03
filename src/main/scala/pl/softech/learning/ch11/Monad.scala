package pl.softech.learning.ch11

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] =
    flatMap(fa) { a =>
      flatMap(fab)(f => pure(f(a)))
    }
}

object Monad extends Ex3.MonadCombinators with Ex4.MonadCombinators with Ex6.MonadCombinators {
  def apply[F[_] : Monad]: Monad[F] = implicitly[Monad[F]]

  def product[A, B, F[_] : Monad](ma: F[A], mb: F[B]): F[(A, B)] = Monad[F].map2(ma, mb)((_, _))
}

object MonadSyntax {

  implicit class MonadOps[F[_], A](val fa: F[A]) extends AnyVal {

    def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)

    def >>=[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)

    def map[B](f: A => B)(implicit F: Monad[F]): F[B] = F.map(fa)(f)
  }

}

object MonadInstances extends Ex1.MonadInstances with Ex2.MonadInstances