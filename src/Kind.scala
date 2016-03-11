package compy

import scala.util.matching.Regex

object Kind {
  val eof = new Kind('EOF, None)
}

// This is a very compassionate class
class Kind(val name: Symbol, val regex: Option[Regex]) {
  
  override def toString: String = name.toString
}
