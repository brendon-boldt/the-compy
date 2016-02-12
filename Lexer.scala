package compy

import scala.util.matching.Regex.Match
import scala.collection.mutable.ArrayBuffer

class Lexer(val grammar: Grammar) {
  // A string which will contain the source file bytes
  var string = ""
  // Number of exactly what you would think
  var errors = 0

  /** Filters token stream of unwanted tokens.
   *  
   *  Removes identifiers that are actually keyword and whitespace tokens.
   */
  private def filterTokens(tokens: ArrayBuffer[Token]): ArrayBuffer[Token] = {
    val filtered = new ArrayBuffer[Token]
    // Tracks where in the source file the tokens are
    var index = 0 
    // Tracks the line number for error reporting
    var line = 1
    for ( token <- tokens ) {
      // Set the token's line number
      token.line = line
      if ( token.kind.name == "newline" ) {
        line += 1
      }
      if (index <= token.start) {
        // Illegal characters such as @ or # will cause a gap in the token stream.
        // This means the index will be less than the start of the next token.
        if (index < token.start) {
          println("Unrecognized token " + string(index) + " at line " + token.line)
          index = token.start
          errors += 1
        }
        // Only add a token if it is not whitespace
        if (!(token.kind.name == "ws" || token.kind.name == "newline"))
          filtered += token
        // Advance the index
        index += token.length
      }
    }
    filtered
  }

  /** Processes the source file string using the RegEx-definied grammar.
   *  Maybe use a stream instead.
   */
  def getTokenArray: Array[Token] = {
    // Reset the number of errors
    errors = 0
    var tokens = new ArrayBuffer[Token]
    // Loop through each of the kinds as defined by the grammar
    for ( k <- grammar.kinds ) {
      // Save all RegEx matches
      var matched = k.regex.findAllMatchIn(string)
      matched.foreach((m: Match) => {
        // Create a token from the match
        var token = new Token(k, m)
        tokens += token
      })
    }
    // Sort the token by position in the source file
    val sorted = tokens.sortBy(_.start)
    // Filter the token stream and return the iterator
    return filterTokens(sorted).toArray
  }

}
