package pl.softech.learning.ch11

import pl.softech.learning.ch11.Assertion._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._

object Ex17 {

  def main(args: Array[String]): Unit = {

    def inc[F[_] : Monad](fa: F[Int]): F[Int] = for {
      a <- fa
    } yield a + 1

    inc(Id(1)) === Id(2)

    inc(List(1)) === List(2)

    inc(Option(1)) === Option(2)
    
    inc(Stream(1)) === Stream(2)

  }

}
