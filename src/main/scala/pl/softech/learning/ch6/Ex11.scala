package pl.softech.learning.ch6

object Ex11 {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.sequence(inputs.map(simulateMachine(_))).map(_.last)

  def simulateMachine(input: Input): State[Machine, (Int, Int)] = for {
    _ <- State.modify[Machine] { s =>
      if (s.candies == 0) s
      else input match {
        case Coin if s.locked => Machine(false, s.candies, s.coins + 1)
        case Turn if !s.locked => Machine(true, s.candies - 1, s.coins)
        case _ => s
      }
    }
    m <- State.get[Machine]
  } yield (m.coins, m.candies)

  def main(args: Array[String]): Unit = {

    val init = Machine(true, 5, 10)
    val inputs = List.fill(4)((Coin, Turn)).flatMap(p => List(p._1, p._2))
    println(inputs)
    println(simulateMachine(inputs).run(init))

  }

}
