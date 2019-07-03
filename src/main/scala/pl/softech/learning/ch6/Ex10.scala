package pl.softech.learning.ch6

import pl.softech.learning.ch6.Ex9.map
import pl.softech.learning.ch6.Ex9.map2
import pl.softech.learning.ch6.RNG.int

object Ex10 {

  type Rand[A] = State[RNG, A]

  val rndInt = new Rand[Int](_.nextInt)

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)
    println(map(int)(_ + 1)(rng))
    println(rndInt.map(_ + 1).run(rng))
    println(rndInt.run(rng))
    println(map2(int, int)(_ + _)(rng))
    println(rndInt.map2(rndInt)(_ + _).run(rng))
    println(rndInt.map2(rndInt)((_, _)).run(rng))
    println(State.unit(1).run(rng))

    println(State.sequence(List()).run(rng))
    println(State.sequence(List(rndInt, rndInt)).run(rng))

  }

}
