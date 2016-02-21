package compy

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer


class Parser(val grammar: Grammar) {

  var tokenArray = Array[Token]()
  var tokenIndex = 0
  var rootNode = new Node('empty, Array.empty[Node])
  var expectedSymbol = 'epsilon
  var error = false

  def produceError(eType: Symbol) = {
    // Add match stuff here
    //match
  }

  def advanceToken(): Boolean = {
    if (tokenIndex + 1 >= tokenArray.size)
      return false
    else {
      tokenIndex += 1
      return true
    }
  }
  
  def backtrack(x: Int) = (tokenIndex -= x)

  def currentToken(): Token = return tokenArray(tokenIndex)

  def buildTree(array: Array[Token]) {
    tokenArray = array
    tokenIndex = 0
    rootNode = new Node('Program, Array.empty[Node])
    if (constructNode(rootNode) == None) {
      println("An error occured while parsing.")
    }
  }

  def constructNode(node: Node): Option[Node] = {
    val s = node.symbol
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
    prods.foreach((a: Array[Symbol]) => {
      var n = applyProduction(node,a)
      if (n != None) {
        return n 
      }
    })
    return None
  }

  def applyProduction(node: Node, a: Array[Symbol]): Option[Node] = {
      val i = a.iterator
      val children = ArrayBuffer.empty[Node]
      if (!i.hasNext) {
        node.setChildren(Array(new Node('epsilon, Array.empty[Node])))
        return Some(node)
      }
      while (i.hasNext) {
        val symbol = i.next
        if (symbol == currentToken.kind.name) {
          // Handle end of token stream
          children += new Node(symbol, Array.empty[Node])
          children.last.value = Some(currentToken)
          if (i.hasNext) {
            if (!advanceToken) {
              println("Parse error: reached end of token stream")
              println(tokenIndex)
              return None
            }
          } else {
            advanceToken
            node setChildren children.toArray
            return Some(node)
          }
        } else {
          expectedSymbol = symbol
          var resultNode = constructNode(new Node(symbol, Array.empty[Node]))
          if (resultNode != None) {
            children += resultNode.get
            if (!i.hasNext) {
              node setChildren children.toArray
              return Some(node)
            }
          } else {
            if (!children.isEmpty && !error) {
              println("Parse Error: expecting " + Grammar.getLiteral(symbol) + " got "
                  + currentToken.string + " on line " + currentToken.line)
              // Maybe add error recovery
              //println("Backtracking: " + children.map(_.getLength).reduce(_+_))
              error = true
              backtrack(children.map(_.getLength).reduce(_+_))
            }
            // Deplete the iterator
            i.size
          }
        }
      }
      return None
  }

}
