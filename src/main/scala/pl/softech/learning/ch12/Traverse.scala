package pl.softech.learning.ch12

import pl.softech.learning.ch10.{Foldable, Monoid}
import pl.softech.learning.ch11.ApplicativeInstances._
import pl.softech.learning.ch11.{Applicative, Functor, MonadInstances}
import pl.softech.learning.ch12.Const.Const
import pl.softech.learning.ch6.State

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  self =>

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[Const[M, *], A, Nothing](as)(f)(monoidApplicativeInstance(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, *], A, B](fa)(f)(MonadInstances.stateMonadInstance)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa) { a =>
      for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b
    }.run(s)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def zipWithIndex2[A](fa: F[A]): F[(A, Int)] =
    traverseS(fa) { a =>
      for {
        i <- State.get[Int]
        _ <- State.set(i + 1)
      } yield (a, i)
    }.runA(0)

  def toList[A](fa: F[A]): List[A] = mapAccum(fa, List.empty[A])((a, as) => ((), a :: as))._2.reverse

  def toList2[A](fa: F[A]): List[A] =
    traverseS(fa) { a =>
      for {
        as <- State.get[List[A]]
        _ <- State.set(a :: as)
      } yield ()
    }.runS(List.empty[A]).reverse

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = mapAccum(fa, toList(fb)) {
    case (a, h :: t) => ((a, Option(h)), t)
    case (a, Nil) => ((a, None), Nil)
  }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = mapAccum(fb, toList(fa)) {
    case (b, a :: as) => ((Option(a), b), as)
    case (b, Nil) => ((None, b), Nil)
  }._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

}

object Traverse {
  def apply[F[_] : Traverse]: Traverse[F] = implicitly[Traverse[F]]
}

object TraverseInstances {

  implicit val listTraverseInstance: Traverse[List] = new Traverse[List] {

    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {

      val G = Applicative[G]

      val gl: G[List[B]] = fa.foldLeft(G.pure(List.empty[B])) { (acc, a) =>
        G.map2(f(a), acc)(_ :: _)
      }

      G.map(gl)(_.reverse)

    }

  }

  implicit val optionTraverseInstance: Traverse[Option] = new Traverse[Option] {

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {

      val G = Applicative[G]

      fa.map(f) match {
        case Some(gb) => G.map(gb)(Option.apply)
        case None => G.pure(None)
      }

    }
  }

  implicit val treeTraverseInstance: Traverse[Tree] = new Traverse[Tree] {

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.head), fa.tail.map(tree => map(tree)(f)))

    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {

      val G = Applicative[G]

      val gb: G[B] = f(fa.head)

      val gbs: List[G[Tree[B]]] = fa.tail.map(t => traverse(t)(f))

      val xs: G[Tree[B]] = G.map2(G.sequence(gbs), gb) { (bs, b) =>
        Tree(b, bs)
      }

      xs
    }
  }


}