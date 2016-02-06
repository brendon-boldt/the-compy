package compy

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val g = new Grammar
    g.addKind("type",     """\b(int|string|boolean)\b""".r)
    g.addKind("int",      """(\b0|[1-9][\d]*\b)""".r)
    g.addKind("boolval",  """(\btrue|false\b)""".r)
    g.addKind("ctrlflow", """(\bif|while\b)""".r)
    g.addKind("symbol",   """[\(\)\{\}=\+\$]|(==)|(!=)""".r)
    /*
    g.addKind("if",       """\bif\b""".r)
    g.addKind("while",    """\bwhile\b""".r)
    g.addKind("lparen",   """\(""".r)
    g.addKind("rparen",   """\)""".r)
    g.addKind("lbracket", """\{""".r)
    g.addKind("rbracket", """\}""".r)
    g.addKind("assign",   """=""".r)
    g.addKind("eq",       """==""".r)
    g.addKind("neq",      """!=""".r)
    g.addKind("plus",     """\+""".r)
    g.addKind("eop",      """\$""".r)
    */
    g.addKind("print",    """(print\((.*)\))""".r)
    g.addKind("stringlit","""(\"([^"]*)\")""".r)
    g.addKind("newline",  """\n""".r)
    g.addKind("id",       """\b([A-z][\w]*)\b""".r)
    g.addKind("ws",       """[\r\t\f ]+""".r)
    val l = new Lexer(g)

    l.string = Source.fromFile(args(0)).toStream.mkString
    l.getTokenIterator
    //l.getTokenIterator.foreach((s:Token) => println(s.string))
  }
}
