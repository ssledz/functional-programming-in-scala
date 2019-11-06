package pl.softech.learning.ch12

import java.util.Date

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch11.ApplicativeInstances._


object Ex6 {

  trait ApplicativeInstances {

    implicit def validationApplicativeInstance[E]: Applicative[Validation[E, *]] = new Applicative[Validation[E, *]] {
      override def pure[A](a: A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Failure(lh, lt), Failure(rh, rt)) => Failure(lh, (lt :+ rh) ++ rt)
          case (_, fail@Failure(_, _)) => fail
          case (fail@Failure(_, _), _) => fail
          case (Success(l), Success(r)) => Success(f(l, r))

        }
    }

  }

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case _ => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  val F = Applicative[Validation[String, *]]

  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =
    F.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(WebForm.apply)

  def main(args: Array[String]): Unit = {

    validWebForm("Slavik", "1990-01-01", "1234567890") ===
      Success(WebForm("Slavik", new Date(90, 0, 1), "1234567890"))


    validWebForm("", "1890-01-01", "1234567") ===
      Failure("Phone number must be 10 digits", tail = Vector("Name cannot be empty"))
  }

}
