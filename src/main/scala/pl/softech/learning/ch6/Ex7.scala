package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG.Implicits._
import pl.softech.learning.ch6.RNG._

object Ex7 {

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val res = fs.foldLeft((List.empty[A], rng)) {
        case ((acc, r), e) => {
          val (x, r2) = e(r)
          (x :: acc, r2)
        }
      }
      (res._1.reverse, res._2)
    }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def main(args: Array[String]): Unit = {
    println(SimpleRNG(1).ints(5))
    println(ints(5)(SimpleRNG(1)))
  }

}
