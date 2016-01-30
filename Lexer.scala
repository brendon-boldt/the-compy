package compy

import scala.util.matching.Regex.Match

class Lexer(val grammar: Grammar) {
  var stream = Stream[Char]()

  def getNextToken: Token = {
    var string = stream.mkString
    // Maybe use hashmap to keep track of order?
    for ( k <- grammar.kinds ) {
      var matched = k.regex.findAllMatchIn(string)
      println("=" + k + "=")
      matched.foreach((m: Match) => println(m.start + " -> " + m))
      /*
      var matched = k.regex.findFirstMatchIn(string)
      if (matched != None)
        println(matched)
        return new Token(k, matched.get)
      */
    }
    return Token.unidentified
  }
}
