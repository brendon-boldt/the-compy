package compy

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val g = new Grammar
    g.addKind("id","""(\b[A-z][\w]*)""".r)
    g.addKind("int","""\b(0|[1-9][\d]*)\b""".r)
    g.addKind("lparen","""\(""".r)
    g.addKind("rparen","""\)""".r)
    g.addKind("lbracket","""\{""".r)
    g.addKind("rbracket","""\}""".r)
    g.addKind("assign","""[^=]=[^=]""".r)
    g.addKind("stringlit","""\"([^"]*)\"""".r)
    val l = new Lexer(g)
    l.stream = Source.fromFile(args(0)).toStream
    //var sb = .addString(new StringBuilder(256))
    //println(l.getNextToken("a2 3 bb"))
    println(l.getNextToken)
  }
}
