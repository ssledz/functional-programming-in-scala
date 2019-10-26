package pl.softech.learning.ch10

import pl.softech.learning.ch10.Monoid._

object Ex10 {

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def zerOne(x: String): Int = if (x.trim.isEmpty) 0 else 1

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def zero: WC = Part("", 0, "")

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l, wc, r), Stub(cs)) => Part("", wc + zerOne(l), r + cs)
      case (Part(l1, wc1, r1), Part(l2, wc2, r2)) => Part("", wc1 + wc2 + zerOne(l1) + zerOne(r1 + l2), r2)
    }
  }

  def main(args: Array[String]): Unit = {

    val txt = "lorem ipsum dolor sit amet,"

    println(txt)

    val f: String => WC = txt => {
      val words = txt.split(" ", -1)
      val wc = words.length
      if (wc <= 1) {
        Stub(txt)
      } else {
        val fst = words(0)
        val last = words(wc - 1)
        val wcc = words.foldLeft(0) { (acc, x) =>
          if (x.isEmpty) acc else acc + 1
        }

        Part(fst, wcc - zerOne(fst) - zerOne(last), last)
      }
    }

    for (window <- 1 to 13) {
      println(s"\nwindow: $window")
      val chunks = (txt + " ").sliding(window, window).toList
      println(chunks.mkString("['", "','", "']"))
      println(chunks.map(f))
      val res = foldMap(chunks, wcMonoid)(f)
      println(res)
      assert(res == Part("", 5, ""))
    }
  }

}
