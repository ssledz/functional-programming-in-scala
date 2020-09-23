package pl.softech.learning.ch9

import pl.softech.learning.ch9.Parsers.{Location, ParseError}
import pl.softech.learning.ch9.ParsersImpl.{Failure, Parser, Result, Success}

import scala.util.matching.Regex

class ParsersImpl extends Parsers[Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  def run2[A](p: Parser[A])(input: String): Result[A] = p(Location(input, 0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  implicit def string(s: String): Parser[String] =
    (loc: Location) =>
      if (loc.input.startsWith(s))
        Success(s, loc.offset + s.length)
      else
        Failure(loc.toError("Expected: " + s))

  def slice[A](p: Parser[A]): Parser[String] = ???

  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  implicit def regex(r: Regex): Parser[String] =
    (loc: Location) => r.findFirstIn(loc.input) match {
      case Some(value) => Success(value, loc.offset + value.length)
      case None => Failure(loc.toError("Expected: " + r.toString()))
    }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???
}

object ParsersImpl {

  type Parser[+A] = Location => Result[A]

  trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

}
