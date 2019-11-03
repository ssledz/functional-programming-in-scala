package pl.softech.learning.ch11

import pl.softech.learning.ch11.Assertion._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._

object Ex12 {

  def join[A, F[_] : Monad](ffa: F[F[A]]): F[A] = for {
    fa <- ffa
    a <- fa
  } yield a

  def main(args: Array[String]): Unit = {

    join(List(List(1, 2, 3), List(4, 5))) === List(1, 2, 3, 4, 5)

    join(List(List(1, 2, 3), List.empty, List(4))) === List(1, 2, 3, 4)

    join(Option(Option(1))) === Option(1)

    join(Option(None: Option[Int])) === None
    
    join(None: Option[Option[Int]]) === None

  }

}
