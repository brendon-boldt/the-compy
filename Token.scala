package compy

import scala.util.matching.Regex.Match

class Token(val kind: Kind, val regMatch: Match) {
  
  // Regex matches can be used to distinguish between
  // int, string, and boolean, for example, since they are all
  // grammatically identical but semantically different.
  var value = "" 
  var string = "" 

  var start = 0
  var length = 0
  var line = 0
  
  def initialize = {
    if (regMatch.groupCount > 0)
      value = regMatch.group(1)
    string = regMatch.group(0)
    start = regMatch.start
    length = string.length//regMatch.end - regMatch.start
  }


  override def toString: String = {
    "<" + kind + ", " + string + ", " + start + ">"
  }
}

