package compy

import scala.util.matching.Regex

object Kind {
}

class Kind(val name: Symbol, val regex: Regex) {
  
  override def toString: String = name.toString
}
