package pl.softech.learning.ch9

import pl.softech.learning.ch9.Parsers.{Location, ParseError}
import pl.softech.learning.ch9.ParsersImpl.{Failure, Parser, Result, Success}

import scala.util.matching.Regex

class ParsersImpl extends Parsers[Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  def run2[A](p: Parser[A])(input: String): Result[A] = p(Location(input, 0))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    loc =>
      p1(loc) match {
        case Failure(_) => p2(loc)
        case other => other
      }

  implicit def string(s: String): Parser[String] =
    (loc: Location) =>
      if (loc.input.startsWith(s))
        Success(s, s.length)
      else
        Failure(loc.toError("Expected: " + s))

  def slice[A](p: Parser[A]): Parser[String] = loc =>
    p(loc) match {
      case Success(_, charsConsumed) => Success(loc.input.take(charsConsumed), charsConsumed)
      case err@Failure(_) => err
    }

  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  implicit def regex(r: Regex): Parser[String] =
    (loc: Location) => r.findPrefixOf(loc.input) match {
      case Some(value) => Success(value, value.length)
      case None => Failure(loc.toError("Expected: " + r.toString()))
    }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)
}

object ParsersImpl {

  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

}
