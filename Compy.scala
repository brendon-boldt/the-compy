package compy

import scala.io.Source

object Main {

  var flagBrackets = false
  var flagVerbose = false

  def main(args: Array[String]) {
    if (!parseOptions(args))
      // If one of the options is not recognized, terminate the program
      return
    var g = new Grammar
    /*
     * The grammar (for lex and parse) is completely specified by
     * the two functions found below.
    */
    g = generateKinds(g)
    g = generateRules(g)
    val l = new Lexer(g)
    l.flagVerbose = flagVerbose
    l.string = Source.fromFile(args.last).toStream.mkString
    val tokenArray = l.getTokenArray
    if (flagVerbose) {
      // Print the token steam (after it has been stripped of whitespace, etc.
      println("Token Steam:")
      tokenArray.foreach((t: Token) => println(t))
      println
    }
    if (l.errors > 0)
      return
    val p = new Parser(g)
    p.flagVerbose = flagVerbose
    p.setTokenStream(tokenArray)
    while (!p.isEOS) {
      // Keep parsing new programs while there are tokens left in the token stream
      p.parseTokens
      if (!p.error && flagBrackets)
        // See the README for use of this output
        println("[" + p.rootNode.getTreeBrackets + "]")
    }
  }

  /**
   * Parse posix-style command line options
   */
  private def parseOptions(args: Array[String]): Boolean = {
    for (option <- args.take(args.size-1)) {
      if (option.take(2) == "--") {
        option.substring(2,option.length) match {
          case "brackets" => flagBrackets = true
          case "verbose" => flagVerbose = true
          case _ => {
            println("Unknown option " + option.substring(2,option.length))
            return false
          }
        }
      } else if (option.head == '-') {
        option.tail.foreach((c:Char) => {
          c match {
            case 'b' => flagBrackets = true
            case 'v' => flagVerbose = true
            case _ => {
              println("Unknown option " + c)
              return false
            }
          }
        })
      } else
        return false
    }
    true
  }

  /**
   * This is where all of the parsing rules are specified.
   * All languge-specific code is set here.
   */
  def generateRules(g: Grammar): Grammar = {
    g.addRule('Program, Array(Array('Block, 'eop)))
    g.addRule('Block, Array(Array('lbracket, 'StatementList, 'rbracket)
      ))
    g.addRule('StatementList, Array(Array('Statement, 'StatementList), Array()))
    g.addRule('Statement, Array(
      Array('Block),
      Array('PrintStatement),
      Array('AssignStatement),
      Array('VarDecl),
      Array('WhileStatement),
      Array('IfStatement)
      ))
    g.addRule('PrintStatement, Array(Array('print, 'lparen, 'Expr, 'rparen)))
    g.addRule('AssignStatement, Array(Array('id, 'assign, 'Expr)))

    g.addRule('Expr, Array(Array('IntExpr), Array('StringExpr), Array('BooleanExpr), Array('id)))
    g.addRule('VarDecl , Array(Array('type, 'id)))
    g.addRule('WhileStatement , Array(Array('while, 'BooleanExpr, 'Block)))
    g.addRule('IfStatement , Array(Array('if, 'BooleanExpr, 'Block)))
    g.addRule('IntExpr , Array(Array('digit),Array('digit, 'intop, 'Expr)))
    g.addRule('StringExpr , Array(Array('stringlit)))
    g.addRule('BooleanExpr , Array(Array('lparen, 'Expr, 'BoolOp, 'Expr, 'rparen), Array('boolval)))
    g.addRule('BoolOp , Array(Array('eq), Array('neq)))
    //g.addRule(' , Array(Array()))
    g
  }

  /**
   * Here is the definition of what will be lexed by--er--lex
   */
  def generateKinds(g: Grammar): Grammar = {
    g.addKind('print,    """(print)""".r)
    g.addKind('type,     """(int|string|boolean)""".r)
    g.addKind('boolval,  """(true|false)""".r)
    g.addKind('digit,    """(\d)""".r)
    g.addKind('intop,    """\+""".r)
    g.addKind('if,       """(if)""".r)
    g.addKind('while,    """(while)""".r)
    g.addKind('lparen,   """\(""".r)
    g.addKind('id,       """([a-z])""".r)
    g.addKind('rparen,   """\)""".r)
    g.addKind('lbracket, """\{""".r)
    g.addKind('rbracket, """\}""".r)
    g.addKind('eq,       """==""".r)
    g.addKind('neq,      """!=""".r)
    g.addKind('assign,   """=""".r)
    g.addKind('eop,      """\$""".r)
    g.addKind('stringlit,"""(\"([a-z ]*)\")""".r)
    // Used in counting newlines for compiler messages
    g.addKind('newline,  """\n""".r)
    // Non newline whitespace
    g.addKind('ws,       """[\r\t\f ]+""".r)
    // Any character/pattern that is not among those above will cause lex to fail.
    g
  }
}
