package compy

import scala.util.matching.Regex.Match
import scala.collection.mutable.ArrayBuffer

class Lexer(val grammar: Grammar) {

  var flagVerbose = false
  def vPrint(s: String) = {
    if (flagVerbose)
      println("LEXER: " + s)
  }
  
  // A string which will contain the source file bytes
  var sourceString = ""
  var programStrings = Array.empty[String]

  // Fix the way that errors are reported
  var errors = 0

  private def tokenError(sourceString: String, line: Int) = {
    println("Unrecognized token " +
      sourceString + " at line " + line)
  }

  var line = 1
  /** 
   *  Filters token stream of unwanted tokens.
   *  Removes identifiers that are actually keyword and whitespace tokens.
   */
  private def filterTokens(tokens: ArrayBuffer[Token], string: String): ArrayBuffer[Token] = {
    var localError = false
    val filtered = ArrayBuffer.empty[Token]
    var index = 0 
    for ( token <- tokens ) {
      token.line = line
      if ( token.kind.name == 'newline ) {
        line += 1
      }
      if (index <= token.start) {
        if (index < token.start) {
          tokenError(string.substring(index, token.start), token.line)
          index = token.start
          errors += 1
          localError = true
        }
        if (!(token.kind.name == 'ws || token.kind.name == 'newline))
          filtered += token
        index += token.length
      }
    }
    if (localError)
      return ArrayBuffer.empty[Token]
    else
      filtered
  }

  /** 
   *  Processes the source file sourceString using the RegEx-definied grammar.
   *  Maybe use a stream instead.
   */
  def getTokenArrays: Array[Array[Token]] = {
    // Reset the number of errors
    errors = 0
    programStrings = sourceString.split("[$]",-1)
    if (programStrings.last.trim.isEmpty)
      programStrings = programStrings.slice(0, programStrings.length-1)
    else
      println("Warning: forgotten \"$\" at end of program")
    var tokenArrays = ArrayBuffer.empty[ArrayBuffer[Token]]
    // Loop through each of the kinds as defined by the grammar
    for ( s <- programStrings ) {
      val tokens = ArrayBuffer.empty[Token]
      for ( k <- grammar.kinds ) {
        // Save all RegEx matches
        var matched = k.regex.get.findAllMatchIn(s)
        matched.foreach((m: Match) => {
          // Create a token from the match
          var token = new Token(k, Some(m))
          token.initialize
          tokens += token
        })
      }
      tokenArrays += tokens
    }
    val sorted = tokenArrays.map(_.sortBy(_.start))
    val filtered = ArrayBuffer.empty[(ArrayBuffer[Token], String)]
    for ( i <- Range(0, programStrings.length)) {
      filtered += ((sorted(i), programStrings(i)))
    }
    return filtered.map((x:(ArrayBuffer[Token],String)) => 
        filterTokens(x._1, x._2).toArray).toArray.filter(!_.isEmpty)
  }

}
