package compy

import scala.collection.mutable.HashMap

class TableNode extends HashMap[String, SymbolEntry] {

  var parent: Option[TableNode] = None
}
