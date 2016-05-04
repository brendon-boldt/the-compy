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

/**
 * Represents an entry in the symbol table; keeps track of line
 * numbers, number of uses, initialization, etc.
 */
class SymbolEntry (val token: Token, val varType: Symbol) {

  val varString = SymbolEntry.getTypeString(varType)
  var uses = 0

  def isUsed = uses > 1
  
  var initialized = false


  override def toString(): String = {
    varType.toString + ":" + token.value
  }
}
