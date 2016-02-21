package compy

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer


class Parser(val grammar: Grammar) {

  var tokenArray = Array[Token]()
  var tokenIndex = 0
  var rootNode = new Node('empty, Array.empty[Node])
  var expectedSymbol = 'epsilon
  var error = false
  var errorString = ""


  def setTokenStream(array: Array[Token]) = {
    tokenArray = array
  }

  def resetTokenStream = {
    tokenIndex = 0
  }

  // Add multi-program parsing
  def parseTokens = {
    error = false
    rootNode = new Node('Program, Array.empty[Node])
    if (constructNode(rootNode) == None) {
      do {
        advanceToken
      } while (!isEOS && currentToken.kind.name != 'eop)
      advanceToken
      println(errorString.stripLineEnd)
    } else {
      errorString = ""
      error = false
    }
  }

  private def tokenError(symbol: Symbol) =  {
    errorString += ("Parse Error: expecting " + Grammar.getLiteral(symbol) + " got "
                  + currentToken.string + " on line " + currentToken.line + "\n")
  }

  private def genericError = {errorString += "Parse Error: an error occured while parsing\n"}

  private def eosError(symbol: Symbol) = {
    errorString = "Parse Error: end of token steam reached; expecting " + 
      Grammar.getLiteral(symbol) + "\n"
  }

  def isEOS(): Boolean = (tokenIndex + 1 >= tokenArray.size)

  private def advanceToken(): Boolean = {
    if (tokenIndex + 1 >= tokenArray.size)
      return false
    else {
      tokenIndex += 1
      return true
    }
  }
  
  private def backtrack(x: Int) = (tokenIndex -= x)

  private def currentToken(): Token = return tokenArray(tokenIndex)

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

  private def applyProduction(node: Node, a: Array[Symbol]): Option[Node] = {
      val i = a.iterator
      val children = ArrayBuffer.empty[Node]
      if (!i.hasNext) {
        node.setChildren(Array(new Node('epsilon, Array.empty[Node])))
        return Some(node)
      }
      while (i.hasNext) {
        val symbol = i.next
        if (symbol == currentToken.kind.name) {
          children += new Node(symbol, Array.empty[Node])
          children.last.value = Some(currentToken)
          if (i.hasNext) {
            if (!advanceToken) {
              eosError(i.next)
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
              error = false
              return Some(node)
            }
          } else {
            if (!children.isEmpty && !error) {
              if (isEOS)
                eosError(symbol)
              else
                tokenError(symbol)
              // Maybe add error recovery by jumping to next block
              // This error might be causing problems
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
