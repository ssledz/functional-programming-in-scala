package pl.softech.learning.ch4

import pl.softech.learning.ch4.Either.Try

object Ex6 {

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 1.1

  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try(age.toInt)
      tickets <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)


  def main(args: Array[String]): Unit = {
    println(parseInsuranceRateQuote("11", "1"))
    println(parseInsuranceRateQuote("11", "1.1"))
    println(parseInsuranceRateQuote("11.1", "1"))

  }

}
