package compy

object Main {
  def main(args: Array[String]) {
    val g = new Grammar
    g.addKind("id","""(\b[A-z][\w]*)""".r)
    g.addKind("int","""\b(0|[1-9][\d]*)\b""".r)
    println(g.getNextToken("a2 3 bb"))
  }
}
