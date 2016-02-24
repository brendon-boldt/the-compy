package compy

import scala.util.matching.Regex

// This a very compassionate class
class Kind(val name: Symbol, val regex: Regex) {
  
  override def toString: String = name.toString
}
