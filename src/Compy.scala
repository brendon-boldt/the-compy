package compy

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {

  var flagBrackets = false
  var flagVerbose = false
  
  // I might not need this variable
  //var errorState = false

  def main(args: Array[String]) {
    if (!parseOptions(args))
      return

    val grammar = generateGrammar

    val lexer = new Lexer(grammar)
    lexer.flagVerbose = flagVerbose
    lexer.string = Source.fromFile(args.last).toStream.mkString
    val tokenArray = lex(lexer)
    if (tokenArray.isEmpty) return ()

    val parser = new Parser(grammar)
    parser.flagVerbose = flagVerbose
    parser.setTokenStream(tokenArray.get)

    val parseTrees = parse(parser)
    if (parseTrees.isEmpty) return ()

    val builder = new ASTBuilder(parseTrees(0))
    println(builder.buildAST)
    
    // Do something with the return value
    //analyze(parseTrees)
  }

  private def analyze(parseTrees: Array[Node]): Boolean = {
    for ( t <- parseTrees ) {
      val analyzer = new Analyzer(t)
      analyzer.analyzeTree
      println(analyzer.rootNode.getSTString())
      if (analyzer.errorState == true) {
        println(analyzer.errorString)
      }
    }
    return false
  }

  private def parse(parser: Parser): Array[Node] = {
    val parseTrees = ArrayBuffer.empty[Node]
    while (!parser.isEOS) {
      parser.parseTokens
      if (!parser.errorState)
        parseTrees += parser.rootNode
      if (!parser.errorState && flagBrackets) {
        println("[" + parser.rootNode.getTreeBrackets + "]")
      } else if (parser.errorState) {
        println("Parsing failed due to one or more errors")
      } else {
        println("Parsing completed successfully")
      }
    }
    return parseTrees.toArray
  }


  private def lex(lexer: Lexer): Option[Array[Token]] = {
    val tokenArray = lexer.getTokenArray
    if (flagVerbose) {
      println("Token Steam:")
      tokenArray.foreach((t: Token) => println(t))
      println
    }

    if (lexer.errors > 0) {
      println("Lexing failed due to one or more errors")
      return None
    } else if (!flagBrackets) {
      println("Lexing completed successfully")
    }
    return Some(tokenArray)
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

  private def generateGrammar(): Grammar = {
    var g = new Grammar
    g = generateKinds(g)
    g = generateRules(g)
    return g
  }

  /**
   * This is where all of the parsing rules are specified.
   * All languge-specific code is set here.
   */
  private def generateRules(g: Grammar): Grammar = {
    g.addRule('Program, Array(Array('Block, 'eop)/*, Array('Block)*/))
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
  private def generateKinds(g: Grammar): Grammar = {
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
