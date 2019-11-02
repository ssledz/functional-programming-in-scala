package pl.softech.learning.ch11

trait Applicative[F[_]] extends Functor[F] {

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

  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] =
    map2(fa, fab) { (a, f) => f(a) }

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

object ApplicativeInstances {

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