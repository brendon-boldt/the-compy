package compy

import scala.collection.mutable.HashMap
import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer

class TableParser(val grammar: Grammar) {

  val table = 

  var tokenArray = Array[Token]()
  var tokens = Iterator[Token]()
  var currentToken: Token = _
  //var rootNode = new Node('empty, Array.empty[Node])
  //var expectedSymbol = 'epsilon

  def advanceToken(): Boolean = {
    if (!tokens.hasNext)
      return false
    currentToken = tokens.next
    return true
  }
}
