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
    val p = new Parser(g)
    p.buildTree(iter)
    println("[" + p.rootNode.getTreeBrackets + "]")
  }

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
    g.addRule('BooleanExpr , Array(Array('lparen, 'BoolOp, 'rparen), Array('boolval)))
    //g.addRule(' , Array(Array()))
    g
  }

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
    g.addKind('assign,   """=""".r)
    g.addKind('eq,       """==""".r)
    g.addKind('neq,      """!=""".r)
    g.addKind('eop,      """\$""".r)
    g.addKind('stringlit,"""(\"([a-z ]*)\")""".r)
    g.addKind('newline,  """\n""".r)
    g.addKind('ws,       """[\r\t\f ]+""".r)
    g
  }
}
