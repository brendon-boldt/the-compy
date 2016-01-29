package compy

import scala.collection.mutable._
import scala.util.matching.Regex

class Grammar {
  val kinds = MutableList[compy.Kind]()

  def addKind(name: String, regex: Regex) {
    kinds += new Kind(name, regex)
  }

  def getNextToken(string: String): Token = {
    for ( k <- kinds ) {
      var value = k.regex.findFirstIn(string)
      if (value != None)
        return new Token(k, value.get)
    }
    return Token.unidentified
  }
}
