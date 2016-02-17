package compy

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    var g = new Grammar
    g = generateKinds(g)
    g = generateRules(g)
    val l = new Lexer(g)
    l.string = Source.fromFile(args(0)).toStream.mkString
    val iter = l.getTokenArray
    iter.foreach((t: Token) => println(t))
    //l.getTokenIterator.foreach((s:Token) => print(s.string))
    //val p = new Parser(g)
    //p.buildTree(iter)
    //println(p.rootNode.getTreeString(0))
    println("[" + p.rootNode.getTreeBrackets + "]")
  }

  def generateRules(g: Grammar): Grammar = {
    g.addRule('Program, Array(Array('Block, 'eop)))
    g.addRule('Block, Array(Array('lbracket, 'StatementList, 'rbracket)
      ))
    g.addRule('StatementList, Array(Array('Statement, 'StatementList), Array()))
    g.addRule('Statement, Array(Array('Block),
      Array('PrintStatement),
      Array('AssignStatement),
      Array('VarDecl),
      Array('WhileStatement),
      Array('IfStatement)
      ))
    g.addRule('PrintStatement, Array(Array('print, 'lparen, 'Expr, 'rparen)))
    g.addRule('AssignStatement, Array(Array('id, 'assign, 'Expr)))

    g.addRule('Expr, Array(Array('IntExpr), Array('StringExpr), Array('BooleanExpr), Array('id)))
    g
  }

  def generateKinds(g: Grammar): Grammar = {
    g.addKind('type,     """\b(int|string|boolean)\b""".r)
    g.addKind('boolval,  """\b(true|false)\b""".r)
    g.addKind('digit,    """\b(\d)\b""".r)
    g.addKind('id,       """\b([a-z])\b""".r)
    g.addKind('intop,    """\+""".r)
    g.addKind('ctrlflow, """(\bif|while\b)""".r)
    g.addKind('lparen,   """\(""".r)
    g.addKind('rparen,   """\)""".r)
    g.addKind('lbracket, """\{""".r)
    g.addKind('rbracket, """\}""".r)
    g.addKind('assign,   """=""".r)
    g.addKind('eq,       """==""".r)
    g.addKind('neq,      """!=""".r)
    g.addKind('eop,      """\$""".r)
    g.addKind('print,    """(print\((.*)\))""".r)
    g.addKind('stringlit,"""(\"([^"]*)\")""".r)
    g.addKind('newline,  """\n""".r)
    g.addKind('ws,       """[\r\t\f ]+""".r)
    g
  }
}
