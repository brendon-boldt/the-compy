package compy

import scala.util.matching.Regex.Match
import scala.collection.mutable.ArrayBuffer

class Lexer(val grammar: Grammar) {
  var string = ""
  var errors = 0

  def removeDuplicates(ab: ArrayBuffer[Token]): ArrayBuffer[Token] = {
    val filtered = new ArrayBuffer[Token]
    var index = 0 
    var line = 1
    for ( token <- ab ) {
      token.line = line
      if ( token.kind.name == "newline" ) {
        line += 1
      }
      if (index <= token.start) {
        if (index < token.start) {
          println("Unrecognized token " + string(index) + " at line " + token.line)
          index = token.start
          errors += 1
        }
        filtered += token
        index += token.length
      }
    }
    filtered
  }

  def getTokenIterator: Iterator[Token] = {
    errors = 0
    var tokens = new ArrayBuffer[Token]
    for ( k <- grammar.kinds ) {
      var matched = k.regex.findAllMatchIn(string)
      matched.foreach((m: Match) => {
        var token = new Token(k, m)
        tokens += token
      })
    }
    val sorted = tokens.sortBy(_.start)
    return removeDuplicates(sorted).iterator
  }
}
