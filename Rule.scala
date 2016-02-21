package compy

object Rule {
  val empty = new Rule('empty,Array(Array.empty[Symbol]))
}

class Rule(val name: Symbol, val arg_productions:Array[Array[Symbol]]) {
  
  val productions = arg_productions.sortBy(-_.length)

  override def toString(): String = {
    var string = "=" + name.toString + "=\n"
    productions.foreach((s: Array[Symbol]) => string += s.mkString+"\n")
    string
  }
}
