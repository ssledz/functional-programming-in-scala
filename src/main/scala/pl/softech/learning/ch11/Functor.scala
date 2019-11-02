package pl.softech.learning.ch11

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  // sometimes called unzip
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }


}

object Functor {
  def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
}

object FunctorSyntax {

  implicit class FunctorOps[F[_], A](val fa: F[A]) extends AnyVal {

    def fmap[B](f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

  }

}

object FunctorInstances {

  implicit val listFunctorInstance: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

}

object FunctorLaws {

  // preserve the structure
  def law1[A, F[_] : Functor](fa: F[A]): (String, Boolean) =
    "Preserves the structure" -> (Functor[F].map(fa)(identity) == fa)

}