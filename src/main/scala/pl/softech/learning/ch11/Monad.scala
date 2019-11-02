package pl.softech.learning.ch11

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] =
    flatMap(fa) { a =>
      flatMap(fab)(f => pure(f(a)))
    }
}

object Monad {
  def apply[F[_] : Monad]: Monad[F] = implicitly[Monad[F]]
}

object MonadSyntax {

  implicit class MonadOps[F[_], A](val fa: F[A]) extends AnyVal {

    def >>=[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)

  }

}

object MonadInstances {

  implicit val listMonadInstance: Monad[List] = new Monad[List] {
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    def pure[A](a: A): List[A] = List(a)
  }

  implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    def pure[A](a: A): Option[A] = Option(a)
  }

}