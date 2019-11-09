package pl.softech.learning.ch12

import pl.softech.learning.ch10.{Foldable, Monoid}
import pl.softech.learning.ch11.ApplicativeInstances._
import pl.softech.learning.ch11.{Applicative, Functor}
import pl.softech.learning.ch12.Const.Const

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[Const[M, *], A, Nothing](as)(f)(monoidApplicativeInstance(mb))

}

object Traverse {
  def apply[F[_] : Traverse]: Traverse[F] = implicitly[Traverse[F]]
}

object TraverseInstances {

  implicit val listTraverseInstance: Traverse[List] = new Traverse[List] {

    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {

      val G = Applicative[G]

      fa.foldRight(G.pure(List.empty[B])) { (a, acc) =>
        G.map2(f(a), acc)(_ :: _)
      }

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

      val xs: G[Tree[B]] = G.map2(gb, G.sequence(gbs)) { (b, bs) =>
        Tree(b, bs)
      }

      xs
    }
  }


}