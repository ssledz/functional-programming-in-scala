package pl.softech.learning.ch11

object Assertion {

  implicit class IdOps[A](val a: A) extends AnyVal {

    def ===(b: A): Unit = {
      if (a != b) {
        println(s"Required : $b but is $a")
      }
      assert(a == b)
    }

  }

}
