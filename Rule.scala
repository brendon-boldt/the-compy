package compy

object Rule {
  val empty = new Rule('empty,Array(Array.empty[Symbol]))
}

class Rule(val name: Symbol, val productions:Array[Array[Symbol]]) {
  
  override def toString(): String = {
    var string = "=" + name.toString + "=\n"
    productions.foreach((s: Array[Symbol]) => string += s.mkString+"\n")
    string
  }
}
