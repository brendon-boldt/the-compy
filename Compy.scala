package compy

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val g = new Grammar
    g.addKind("id",       """\b([A-z][\w]*)\b""".r)
    g.addKind("int",      """\b(0|[1-9][\d]*)\b""".r)
    g.addKind("type",     """\b(int|string|boolean)\b""".r)
    g.addKind("boolval",  """\b(true|false)\b""".r)
    g.addKind("if",       """\bif\b""".r)
    g.addKind("while",    """\bwhile\b""".r)
    g.addKind("lparen",   """\(""".r)
    g.addKind("rparen",   """\)""".r)
    g.addKind("lbracket", """\{""".r)
    g.addKind("rbracket", """\}""".r)
    g.addKind("assign",   """=]""".r)
    g.addKind("eq",       """==""".r)
    g.addKind("neq",      """!=""".r)
    g.addKind("plus",     """\+""".r)
    g.addKind("print",    """print\((.*)\)""".r)
    g.addKind("stringlit","""\"([^"]*)\"""".r)
    g.addKind("eop",      """$""".r)
    //g.addKind("ws",       """\s""".r)
    // How do a catch errors with regex?
    //g.addKind("invalid","""[^\w\(\)\{\}]*""".r)
    val l = new Lexer(g)
    l.stream = Source.fromFile(args(0)).toStream
    //var sb = .addString(new StringBuilder(256))
    //println(l.getNextToken("a2 3 bb"))
    println(l.getNextToken)
  }
}
