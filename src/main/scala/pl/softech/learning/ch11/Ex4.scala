package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._

object Ex4 {

  trait MonadCombinators {

    def replicateM[A, F[_] : Monad](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  }

  def main(args: Array[String]): Unit = {

    replicateM(3, Option(1)) === Some(List(1, 1, 1))

    replicateM(5, None) === None

  }

}
