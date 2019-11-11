package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._
import pl.softech.learning.ch12.TraverseInstances._

object Ex20 {

  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[Lambda[A => F[G[A]]]] = new Monad[Lambda[A => F[G[A]]]] {

    override def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))

    override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
      F.flatMap(fa) { ga =>

        val gfgb: G[F[G[B]]] = G.map(ga)(f)

        val fggb: F[G[G[B]]] = T.sequence(gfgb)

        val fgb: F[G[B]] = F.flatMap(fggb) { ggb =>
          F.pure(G.join(ggb))
        }
        fgb
      }
    }
  }

  type ListOpt[A] = List[Option[A]]

  def main(args: Array[String]): Unit = {

    implicit val F: Monad[ListOpt] = Monad[List].composeM[Option]
    //    implicit val F : Monad[ListOpt] = composeM[List, Option]

    def parseAndAdd[M[_] : Monad](ma: M[String], mb: M[String]): M[Int] = for {
      a <- ma
      b <- mb
    } yield a.toInt + b.toInt

    F.pure(1) === List(Some(1))

    parseAndAdd(F.pure("1"), F.pure("2"))(F) === List(Some(3))

  }

}
