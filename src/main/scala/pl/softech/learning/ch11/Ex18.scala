package pl.softech.learning.ch11

import pl.softech.learning.ch11.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch6.State

object Ex18 {

  type IntState[A] = State[Int, A]

  def main(args: Array[String]): Unit = {

    val inc: IntState[Int] = for {
      counter <- State.get[Int]
      _ <- State.set(counter + 1)
    } yield counter

    val r = replicateM(3, inc)

    inc.run(1) === (1, 2)

    r.run(1) === (List(1, 2, 3), 4)

    val m2 = Monad[IntState].map2(inc, inc) { (a, b) =>
      a + b
    }

    val rm2 = replicateM(3, m2)

    // 1 + 2, 3
    m2.run(1) === (3, 3)

    // 1 + 2, 3
    // 3 + 4, 5
    // 5 + 6, 7
    rm2.run(1) === (List(3, 7, 11), 7)

    val s = sequence[Int, IntState](List(inc, inc, inc))

    s.run(1) === (List(1, 2, 3), 4)

  }

}
