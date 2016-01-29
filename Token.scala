package compy

object Token {
  val unidentified = new Token(new Kind("unidentified", """""".r), "")
}

class Token(val kind: Kind, val value: String) {

  override def toString: String = {
    "<" + kind + ", " + value + ">"
  }
}
