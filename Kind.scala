package compy

import scala.util.matching.Regex

class Kind(val name: Symbol, val regex: Regex) {
  
  override def toString: String = name
}
