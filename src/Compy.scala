package compy

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {

  var flagCST = false
  var flagAST = false
  var flagOAST = false
  var flagST = false
  var flagVerbose = false
  var flagBrackets = false

  val grammar = generateGrammar

  def main(args: Array[String]) {
    if (!parseOptions(args))
      return

    val lexer = new Lexer(grammar)
    lexer.flagVerbose = flagVerbose
    lexer.sourceString = Source.fromFile(args.last).toStream.mkString
    val tokenArrays = lex(lexer)
    // Maybe leave a note if all programs fail? //
    if (tokenArrays.isEmpty) return ()

    val parseTrees = parse(tokenArrays)
    if (parseTrees.isEmpty) return ()

    val astArray = buildASTArray(parseTrees)

    val analyzedArray = analyze(astArray)
    if (analyzedArray.isEmpty) return ()

    val optimizedArray = optimize(analyzedArray)


    for ( exe <- codeGen(optimizedArray) ) {
      println
      println(exe)
      println
    }
  }

  private def codeGen(array: Array[Node]): Array[Executable] = {
    OCTemplate.flagVerbose = flagVerbose
    val executables = ArrayBuffer.empty[Executable]
    for ( ast <- array ) {
      val g = new Generator(ast)
      g.flagVerbose = flagVerbose
      val exe = g.generateExecutable
      if (!exe.isEmpty)
        executables += exe.get
      //println(executables.last.staticTable.mkString("\n"))
    }
    return executables.toArray
  }

  private def optimize(argArray: Array[Node]): Array[Node] = {
    val optimizedArray = ArrayBuffer.empty[Node]
    for ( aa <- argArray ) {
      val opt = new Optimizer(aa)
      optimizedArray += opt.optimizeTree
      if (flagOAST)
        if (flagBrackets)
          println("["+optimizedArray.last+"]")
        else
          println(optimizedArray.last.getTreeString()+"\n")
    }
    return optimizedArray.toArray
  }

  private def analyze(argArray: Array[Node]): Array[Node] = {
    val astArray = ArrayBuffer.empty[Node]
    Analyzer.flagVerbose = flagVerbose
    for ( t <- argArray ) {
      val analyzer = new Analyzer(t)
      analyzer.flagVerbose = flagVerbose
      analyzer.analyzeTree
      if (!analyzer.errorState)
        astArray += t
      if (flagST)
        println(t.getSTString() + "\n")
    }
    return astArray.toArray
  }

  private def buildASTArray(parseTrees: Array[Node]): Array[Node] = {
    val astArray = ArrayBuffer.empty[Node]
    for ( t <- parseTrees ) {
      val builder = new ASTBuilder(t)
      ASTBuilder.flagVerbose = flagVerbose
      builder.flagVerbose = flagVerbose
      astArray += builder.buildAST
      if (flagAST) {
        if (flagBrackets)
          println("[" + astArray.last + "]\n")
        else
          println(astArray.last.getTreeString()+"\n")
      }
    }
    return astArray.toArray
  }

  private def parse(tokenArrays: Array[Array[Token]]): Array[Node] = {
    val parseTrees = ArrayBuffer.empty[Node]
    for ( tokens <- tokenArrays ) {
      val parser = new Parser(grammar)
      parser.flagVerbose = flagVerbose
      parser.setTokenStream(tokens)

      parser.parseTokens
      if (!parser.errorState)
        parseTrees += parser.rootNode

      if (!parser.errorState && flagCST) {
        if (flagBrackets)
          println("[" + parser.rootNode.getTreeBrackets + "]\n")
        else
          println(parser.rootNode.getTreeString()+"\n")
      } else if (parser.errorState) {
        println("Parsing failed due to one or more errors")
      } else {
        println("Parsing completed successfully")
      }
    }
    return parseTrees.toArray
  }

  private def lex(lexer: Lexer): Array[Array[Token]] = {
    val tokenArrays = lexer.getTokenArrays
    if (flagVerbose) {
      println("Token Steams:")
      for ( tokens <- tokenArrays ) 
        tokens.foreach((t: Token) => println(t))
      println
    }

    if (lexer.errors > 0) {
      println("Some programs failed lex\nContinuing compilation of valid programs")
    } else if (flagCST || flagAST) {
      println("Lexing completed successfully")
    }
    return tokenArrays
  }

  /**
   * Parse posix-style command line options
   */
  private def parseOptions(args: Array[String]): Boolean = {
    for (option <- args.take(args.size-1)) {
      if (option.take(2) == "--") {
        option.substring(2,option.length) match {
          case "cst" => flagCST = true
          case "ast" => flagAST = true
          case "oast" => flagOAST = true
          case "st" => flagST = true
          case "verbose" => flagVerbose = true
          case "brackets" => flagBrackets = true
          case _ => {
            println("Unknown option " + option.substring(2,option.length))
            return false
          }
        }
      } else if (option.head == '-') {
        option.tail.foreach((c:Char) => {
          c match {
            case 'c' => flagCST = true
            case 'a' => flagAST = true
            case 'o' => flagOAST = true
            case 's' => flagST = true
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
    g.addRule('Program, Array(Array('Block)))
    //g.addRule('Program, Array(Array('Block, 'eop)/*, Array('Block)*/))
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
    g.addKind('COMMENT,  """//""".r)
    //g.addKind('BCOM,  """/\*""".r)
    //g.addKind('ECOM,  """\*/""".r)
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
