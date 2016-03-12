package compy

import scala.collection.mutable.HashMap

class TableNode extends HashMap[String, SymbolEntry] {

  var parent: Option[TableNode] = None

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
