package compy

import scala.collection.mutable.HashMap

/**
 * A wrapper class for a certain parameterization of a hashmap
 * which includes a nifty toString method
 */
class TableNode extends HashMap[String, SymbolEntry] {

  override def toString(): String = {
    var string = ""
    val i = this.toIterator
    while (i.hasNext) {
      val t = i.next  
      string += t._1 + " -> " + t._2 + "\n"
    }
    string
  }
}
