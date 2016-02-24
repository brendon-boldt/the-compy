package compy

object Rule {
  val empty = new Rule('empty,Array(Array.empty[Symbol]))
}

// Each production is an array of symbols, and since we have multiple poductions
// for a rule, we need an array of an array of symbols. Hence, we can view the grammar
// as a collection of a collection of a collection of symbols.
class Rule(val name: Symbol, val arg_productions:Array[Array[Symbol]]) {
  
  // Productions __must__ be sorted by length (largest to smallest).
  // This is so because when common prefixes occur, we want the longest
  // production to be attempted first so that there are no grammatical
  // ambiguities.
  val productions = arg_productions.sortBy(-_.length)

  override def toString(): String = {
    var string = "=" + name.toString + "=\n"
    productions.foreach((s: Array[Symbol]) => string += s.mkString+"\n")
    string
  }
}
