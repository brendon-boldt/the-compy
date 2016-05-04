package compy

import scala.util.matching.Regex

object Kind {
  val eof = new Kind('EOF, None)
  
  // A token created outside of lex
  val nil = new Kind('NIL, None)
}

// This is a very compassionate class
class Kind(val name: Symbol, val regex: Option[Regex]) {
  
  override def toString: String = name.toString
}
