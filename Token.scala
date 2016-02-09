package compy

import scala.util.matching.Regex.Match

class Token(val kind: Symbol, val regMatch: Match) {
  
  private var tempValue = ""
  if (regMatch.groupCount > 0)
    tempValue = regMatch.group(1)
  val value = tempValue
  val string = regMatch.group(0)

  val start = regMatch.start
  val length = string.length//regMatch.end - regMatch.start
  var line = 0
  
  override def toString: String = {
    "<" + kind + ", " + string + ", " + start + ">"
  }
}
