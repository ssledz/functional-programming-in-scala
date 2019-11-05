package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._

import scala.util.Try

object Ex8 {

  def flatMap[A, B, F[_] : Monad](fa: F[A])(f: A => F[B]): F[B] = {

    val f0: F[A] => F[A] = _ => fa

    val ff: F[A] => F[B] = Monad[F].compose(f0, f)

    ff(fa)
  }


  def main(args: Array[String]): Unit = {

    def parse(x: String): Option[Int] = Try(x.toInt).toOption

    val toList: Option[Int] => List[Int] = _.toList

    flatMap(Option("1"))(parse) === Some(1)

    flatMap(List("1", "2", "3"))(parse _ andThen toList) === List(1, 2, 3)

  }

}
