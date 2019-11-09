package pl.softech.learning.ch12

import pl.softech.learning.ch11.Monad

object Ex11 {

  def compose[F[_], G[_]](implicit F: Monad[F], G: Monad[G]): Monad[λ[A => F[G[A]]]] = new Monad[λ[A => F[G[A]]]] {

    def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))

    private def extract[B](gfgb: G[F[G[B]]]): F[G[B]] = ???

    def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = F.flatMap(fa) { ga =>
      val gfgb: G[F[G[B]]] = G.map(ga) { a =>
        f(a)
      }
      extract(gfgb)
    }
  }
}
