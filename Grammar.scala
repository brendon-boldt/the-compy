package compy

import scala.collection.mutable._
import scala.util.matching.Regex

class Grammar {
  val kinds = MutableList[compy.Kind]()

  def addKind(name: String, regex: Regex) {
    kinds += new Kind(name, regex)
  }

}
