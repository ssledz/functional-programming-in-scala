package pl.softech.learning.ch11

import pl.softech.learning.ch12
import pl.softech.learning.ch12.Traverse

trait Monad[F[_]] extends Applicative[F] {

  self =>

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // it means that map2 is right biased, probably it would be better to have left biased
  override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] =
    flatMap(fa) { a =>
      flatMap(fab)(f => pure(f(a)))
    }

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa) { fa =>
    flatMap(fa)(pure)
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def composeM[G[_]](implicit G: Monad[G], T: Traverse[G]): Monad[Lambda[A => F[G[A]]]] = new Monad[Lambda[A => F[G[A]]]] {

    override def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

    override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
      self.flatMap(fa) { ga =>

        val gfgb: G[F[G[B]]] = G.map(ga)(f)

        val fggb: F[G[G[B]]] = T.sequence(gfgb)(self)

        val fgb: F[G[B]] = self.flatMap(fggb) { ggb =>
          self.pure(G.join(ggb))
        }
        fgb
      }
    }
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

object MonadInstances extends Ex1.MonadInstances with Ex2.MonadInstances with ch12.Ex5.MonadInstances {

  implicit val idMonadInstance: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)

    def pure[A](a: A): Id[A] = Id.pure(a)
  }

  implicit def readerMonad[R]: Monad[Reader[R, *]] = new Monad[Reader[R, *]] {

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = st.flatMap(f)

    def pure[A](a: A): Reader[R, A] = Reader.pure(a)
  }

}