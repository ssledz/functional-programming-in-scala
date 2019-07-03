package pl.softech.learning.ch4

object Ex8 {

  case class Person(name: Name, age: Age)

  sealed case class Name(val value: String)

  sealed case class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def main(args: Array[String]): Unit = {
    println(mkPerson("Alan", 1))
    println(mkPerson("", 1))
    println(mkPerson("Alan", -1))
    println(mkPerson(null, -1))
  }

}
