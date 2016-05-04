package compy

import scala.util.matching.Regex.Match

class Token(val kind: Kind = Kind.nil, val regMatch: Option[Match] = None) {
  
  // Regex matches can be used to distinguish between
  // int, string, and boolean, for example, since they are all
  // grammatically identical but semantically different.
  var value = "" 
  var string = "" 

  var start = 0
  var length = 0
  var line = 0
  
  def initialize = {
    if (regMatch.get.groupCount > 0)
      value = regMatch.get.group(1)
    string = regMatch.get.group(0)
    start = regMatch.get.start
    length = string.length//regMatch.get.end - regMatch.get.start
  }


  override def toString: String = {
    "<" + kind + ", " + string + ", " + start + ">"
  }
}

