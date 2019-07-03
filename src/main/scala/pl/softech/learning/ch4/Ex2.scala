package pl.softech.learning.ch4

object Ex2 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val ys: Option[Seq[Double]] = for {
      m <- mean(xs)
    } yield for {
      x <- xs
    } yield Math.pow(x - m, 2)

    ys.flatMap(mean(_))
  }

  def main(args: Array[String]): Unit = {
    println(variance(Seq(2,2,2,2)))
    println(variance(Seq(1,2,1,2)))
  }

}
