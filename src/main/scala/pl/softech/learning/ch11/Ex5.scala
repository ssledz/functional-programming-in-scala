package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._

object Ex5 {

  def main(args: Array[String]): Unit = {

    replicateM(3, Option(1)) === Some(List(1, 1, 1))
    replicateM(3, None) === None
    replicateM(2, List(1, 2)) === List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))
    replicateM(3, List.empty) === List.empty

  }

}
