package compy

import scala.util.matching.Regex.Match
import scala.collection.mutable.ArrayBuffer

class Lexer(val grammar: Grammar) {

  var flagVerbose = false
  
  // A string which will contain the source file bytes
  var string = ""
  // Number of exactly what you would think
  var errors = 0

  private def tokenError(string: String, line: Int) = {
    println("Unrecognized token " +
      string + " at line " + line)
  }

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
      if ( token.kind.name == 'newline ) {
        line += 1
      }
      // If the 'while' is matched as token, there will also be a token for the inital
      // 'w' as an ID. Matching 'while' moved the index past the index of the 'w' which
      // means that the below condition will be false for the ID 'w' and it will not be
      // included in the token stream.
      if (index <= token.start) {
        // Illegal characters such as @ or # will cause a gap in the token stream.
        // This means the index will be less than the start of the next token.
        if (index < token.start) {
          tokenError(string.substring(index, token.start), token.line)
          index = token.start
          errors += 1
        }
        // Only add a token if it is not whitespace
        if (!(token.kind.name == 'ws || token.kind.name == 'newline))
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
    // Sort the token by position in the source file.
    // Since this is a stable sort (timsort, I think), lex rules which
    // were specified first will still come before subsequent rules, think
    // while as keyword vs while as 5 ID's. This is important for filtering
    // the tokens proprely.
    val sorted = tokens.sortBy(_.start)
    // Filter the token stream and return the iterator
    return filterTokens(sorted).toArray
  }

}
