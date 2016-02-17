package compy

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer


class Parser(val grammar: Grammar) {

  var tokenArray = Array[Token]()
  var tokens = Iterator[Token]()
  var currentToken: Token = _
  var rootNode = new Node('empty, Array.empty[Node])
  var expectedSymbol = 'epsilon

  def advanceToken(): Boolean = {
    if (!tokens.hasNext)
      return false
    currentToken = tokens.next
    return true
  }

  def buildTree(array: Array[Token]) {
    tokenArray = array
    tokens = tokenArray.iterator
    advanceToken
    rootNode = new Node('Program, Array.empty[Node])
    if (f(rootNode) == None) {
      println("Parse Error: expecting " + expectedSymbol + " got " + currentToken)
    }
  }

  def f(node: Node): Option[Node] = {
    val s = node.symbol
    //println("f with " + node)
    var rule = grammar.rules.getOrElse(s, Rule.empty)
    if (rule.name == 'empty) {
      if (s == currentToken.kind.name) {
        advanceToken
        return Some(new Node('epslilon, Array.empty[Node]))
      } else {
        return None
      }
    }

    val prods = rule.productions
    // This might be better off with the charcter -> rule than r -> c
    prods.foreach((a: Array[Symbol]) => {
      val i = a.iterator
      val children = ArrayBuffer.empty[Node]
      if (!i.hasNext) {
        node.setChildren(Array(new Node('epslilon, Array.empty[Node])))
        return Some(node)
      }
      while (i.hasNext) {
        val symbol = i.next
        if (symbol == currentToken.kind.name) {
          // Handle end of token stream
          advanceToken
          children += new Node(symbol, Array.empty[Node])
          if (!i.hasNext) {
            //println("~~"+ a.mkString)
            node setChildren children.toArray
            return Some(node)
          }
        } else {
          expectedSymbol = symbol
          var resultNode = f(new Node(symbol, Array.empty[Node]))
          if (resultNode != None) {
            children += resultNode.get
            if (!i.hasNext) {
              node setChildren children.toArray
              return Some(node)
            }
          } else {
            i.size// Deplete the iterator
          }
        }
      }
    })
    println("Parse error: Expecting " + s.toString)
    return None
  }
}
