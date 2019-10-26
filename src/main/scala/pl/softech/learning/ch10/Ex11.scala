package pl.softech.learning.ch10

import pl.softech.learning.ch10.Ex10._
import pl.softech.learning.ch10.Monoid.foldMap

object Ex11 {

  def wc(txt: String, window: Int = 13): Int = {
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
    val chunks = (txt + " ").sliding(window, window).toList
    val res = foldMap(chunks, wcMonoid)(f)
    res match {
      case Part(_, wc, _) => wc
    }

  }

  def main(args: Array[String]): Unit = {
    assert(wc("") == 0)
    assert(wc("lorem") == 1)
    assert(wc(" lorem ipsum ") == 2)
    assert(wc("lorem ipsum") == 2)
  }

}
