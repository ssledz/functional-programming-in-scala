package pl.softech.learning.ch11

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] =
    flatMap(fa) { a =>
      flatMap(fab)(f => pure(f(a)))
    }

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa) { fa =>
    flatMap(fa)(pure)
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

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

object MonadInstances extends Ex1.MonadInstances with Ex2.MonadInstances {

  implicit val idMonadInstance: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)

    def pure[A](a: A): Id[A] = Id.pure(a)
  }

  implicit def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] = new Monad[({type f[x] = Reader[R, x]})#f] {

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = st.flatMap(f)

    def pure[A](a: A): Reader[R, A] = Reader.pure(a)
  }

}