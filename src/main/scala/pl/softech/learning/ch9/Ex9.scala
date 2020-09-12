package pl.softech.learning.ch9

object Ex9 {

  trait JSON

  object JSON {

    case object JNull extends JSON

    case class JNumber(get: Double) extends JSON

    case class JString(get: String) extends JSON

    case class JBool(get: Boolean) extends JSON

    case class JArray(get: IndexedSeq[JSON]) extends JSON

    case class JObject(get: Map[String, JSON]) extends JSON

  }

  // TODO Implement Parser[JSON]
  def parser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = ???

}
