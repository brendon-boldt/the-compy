package compy

import scala.util.matching.Regex

object Kind {
  val eof = new Kind('EOF, null)
}

// This is a very compassionate class
class Kind(val name: Symbol, val regex: Regex) {
  
  override def toString: String = name.toString
}
