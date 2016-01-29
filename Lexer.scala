package compy

import scala.io.Source

object Lexer {
  def main(args: Array[String]) {
    var sb = Source.fromFile(args(0)).addString(new StringBuilder(256))
    println(sb.toString())
  }
}
