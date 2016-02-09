package compy

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val g = new Grammar
    /*
     * Consolidate this into word/digit/symbol pattern.
     * Interpret match values during duplicate removal
     */
    g.addPattern('word,       """\b(\w+)\b""")
    g.addPattern('digit,      """\b(\d)\b""")
    g.addPattern('symbol,     """[\(\)\{\}=\+\$]|(==)|(!=)""".r)
    g.addPattern('stringlit,  """(\"([^"]*)\")""".r)
    g.addPattern('newline,    """\n""".r)
    g.addPattern('ws,         """[\r\t\f ]+""".r)
    g.addKind('intop,         """\+""".r)
    g.addKind('boolval,       """(false|true)""".r)
    g.addKind('boolop,        """(!=|==)""".r)
    g.addKind('digit,         """\d""".r)
    g.addKind('char,          """[a-z]""".r)
    g.addKind('type,          """(int|string|boolean)""".r)
    // Add while/if/print

    //g.addKind("type",     """\b(int|string|boolean)\b""".r)
    //g.addKind("int",      """(\b0|[1-9][\d]*\b)""".r)
    //g.addKind("boolval",  """(\btrue|false\b)""".r)
    //g.addKind("ctrlflow", """(\bif|while\b)""".r)
    //g.addKind("symbol",   """[\(\)\{\}=\+\$]|(==)|(!=)""".r)

    //g.addKind("print",    """(print\((.*)\))""".r)
    //g.addKind("stringlit","""(\"([^"]*)\")""".r)
    //g.addKind("newline",  """\n""".r)
    //g.addKind("id",       """\b([A-z][\w]*)\b""".r)
    //g.addKind("ws",       """[\r\t\f ]+""".r)
    val l = new Lexer(g)

    l.string = Source.fromFile(args(0)).toStream.mkString
    l.getTokenIterator.foreach((s:Token) => println(s.string))
  }
}
