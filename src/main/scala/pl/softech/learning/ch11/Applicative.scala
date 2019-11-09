package pl.softech.learning.ch11

import pl.softech.learning.ch12

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, pure(()))((a, _) => f(a))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fbc: F[B => C] = ap(fa)(pure(f.curried))
    ap(fb)(fbc)
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fbcd: F[B => C => D] = ap(fa)(pure(f.curried))
    val fcd: F[C => D] = ap(fb)(fbcd)
    ap(fc)(fcd)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fbcde: F[B => C => D => E] = ap(fa)(pure(f.curried))
    val fcde: F[C => D => E] = ap(fb)(fbcde)
    val fde: F[D => E] = ap(fc)(fcde)
    ap(fd)(fde)
  }

  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] =
    map2(fa, fab) { (a, f) => f(a) }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(pure(List.empty[B])) { (a, acc) =>
      map2(f(a), acc)(_ :: _)
    }

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def product[G[_]](implicit G: Applicative[G]): Applicative[Lambda[A => (F[A], G[A])]] = new Applicative[Lambda[A => (F[A], G[A])]] {

    def pure[A](a: A): (F[A], G[A]) = (self.pure(a), G.pure(a))

    override def map2[A, B, C](faa: (F[A], G[A]), fbb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
      val (fa, ga) = faa
      val (fb, gb) = fbb
      (self.map2(fa, fb)(f), G.map2(ga, gb)(f))
    }

  }

}

object Applicative {
  def apply[F[_] : Applicative]: Applicative[F] = implicitly[Applicative[F]]
}

object ApplicativeSyntax {

  implicit class ApplicativeOps[F[_], A](val fa: F[A]) extends AnyVal {

    def ap[B](fab: F[A => B])(implicit F: Applicative[F]): F[B] = F.ap(fa)(fab)

    def map2[B, C](fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map2(fa, fb)(f)

  }

  implicit class Tuple2ApplicativeOps[F[_], A, B](val fab: (F[A], F[B])) extends AnyVal {
    def mapN[C](f: (A, B) => C)(implicit F: Applicative[F]): F[C] = fab match {
      case (fa, fb) => F.map2(fa, fb)(f)
    }
  }

  implicit class Tuple3ApplicativeOps[F[_], A, B, C](val fabc: (F[A], F[B], F[C])) extends AnyVal {
    def mapN[D](f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] = fabc match {
      case (fa, fb, fc) => F.map3(fa, fb, fc)(f)
    }
  }

}

object ApplicativeInstances extends ch12.Ex6.ApplicativeInstances {

  implicit val listApplicativeInstance: Applicative[List] = new Applicative[List] {
    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = for {
      a <- fa
      b <- fb
    } yield f(a, b)

    override def pure[A](a: A): List[A] = List(a)
  }

  implicit val optionApplicativeInstance: Applicative[Option] = new Applicative[Option] {
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = for {
      a <- fa
      b <- fb
    } yield f(a, b)

    override def pure[A](a: A): Option[A] = Option(a)
  }

}