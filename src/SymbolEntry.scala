package compy

object SymbolEntry {
  
  def getTypeString(varType: Symbol): String = {
    varType match {
      case 'int => "int"
      case 'string => "string"
      case 'boolean => "boolean"
      case _ => varType.toString
    }
  }
}


class SymbolEntry (val token: Token, val varType: Symbol) {

  val varString = SymbolEntry.getTypeString(varType)

  override def toString(): String = {
    varType.toString
  }
}
