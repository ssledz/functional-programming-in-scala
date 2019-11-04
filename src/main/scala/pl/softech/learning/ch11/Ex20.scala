package pl.softech.learning.ch11

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad._
import pl.softech.learning.ch11.MonadInstances._

object Ex20 {

  type Env = Map[String, String]

  type EnvReader[A] = Reader[Env, A]

  def main(args: Array[String]): Unit = {

    val F = Monad[EnvReader]

    val one = F.pure(1)

    def addTwoOr0(arg: EnvReader[Int]) = F.flatMap(arg) { x =>
      F.map(Reader.ask[Env]) { env =>
        x + env.getOrElse("two", "0").toInt
      }
    }

    val emptyEnv = Map.empty[String, String]

    addTwoOr0(one).run(emptyEnv) === 1

    addTwoOr0(one).run(Map("two" -> "2")) === 3

    sequence[Int, EnvReader](List(one, addTwoOr0(one), one)).run(emptyEnv) === List(1, 1, 1)

    sequence[Int, EnvReader](List(one, addTwoOr0(one), one)).run(Map("two" -> "2")) === List(1, 3, 1)

    replicateM(3, one).run(emptyEnv) === List(1, 1, 1)

    replicateM(3, addTwoOr0(one)).run(Map("two" -> "2")) === List(3, 3, 3)

    F.join(F.pure(one)).run(emptyEnv) === 1

  }

}
