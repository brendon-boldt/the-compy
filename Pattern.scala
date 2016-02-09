package compy

import scala.util.matching.Regex

class Pattern(val name: Symbol, val regex: Regex) {

  override def toString: String = name
}
