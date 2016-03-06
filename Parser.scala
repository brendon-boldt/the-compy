package compy

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer


class Parser(val grammar: Grammar) {

  var flagVerbose = false

  var tokenArray = Array[Token]()
  var tokenIndex = 0
  var rootNode = new Node('empty, Array.empty[Node])
  var errorState = false
  var eosErrorSymbol: Option[Symbol] = None
  var errorString = ""
  val errors = ArrayBuffer.empty[(Symbol, Token)]

  /**
   * I have not bothered to implement a strea here yet.
   */
  def setTokenStream(array: Array[Token]) = {
    tokenArray = array
  }

  def resetTokenStream = {
    tokenIndex = 0
  }

  /**
   * This parses one program and builds the CST from rootNode
   */
  def parseTokens = {
    errorState = false
    rootNode = new Node('Program, Array.empty[Node])

    var res = constructNode(rootNode)
    errorState = !errors.isEmpty
    errorState = errorState || (!eosErrorSymbol.isEmpty)
    makeErrorString

    if (errorState) {
      if (flagVerbose)
        println("Parse failed; advancing to next program")
      do {
        advanceToken
      } while (!isEOS && currentToken.kind.name != 'eop)
      advanceToken
      println(errorString.stripLineEnd)
    } else {
      if (rootNode.children.length == 1)
        println("Parser warning: forgotten '$' at end of program")
      //resetError
    }
  }

  private def makeErrorString = {
    errors.foreach((arg: (Symbol, Token)) => arg match {
      case (s,t) => errorString += ("Parse Error: expecting " + Grammar.getLiteral(s) +
        " got " + t.string + " on line " + t.line + "\n")
    })
    if (!eosErrorSymbol.isEmpty)
      errorString += ("Parse Error: expecting " + Grammar.getLiteral(eosErrorSymbol) +
        " reached end of stream")
  }

  /**
   * An error generator when an expected token is not found.
   */
  private def tokenError(symbol: Symbol, token: Token) =  {
    errorState = true
    if (token.kind.name == 'EOF)
      eosError(symbol)
    else
      errors += new Tuple2(symbol, token)
    //errorString += ("Parse Error: expecting " + Grammar.getLiteral(symbol) + " got "
    //              + token.string + " on line " + token.line + "\n")
  }

  /**
   * End of stream error -- up the creek without a paddle
   */
  private def eosError(symbol: Symbol) = {
    errorState = true
    eosErrorSymbol = Some(symbol)
    //errorString = "Parse Error: end of token steam reached; expecting " + 
    //  Grammar.getLiteral(symbol) + "\n"
  }

  def resetError = {
    errorState = false
  }

  def isEOS(): Boolean = (currentToken.kind.name == 'EOF)

  /**
   * Advance the token iterator. Return whether it was succesfully advanced.
   */
  private def advanceToken(): Boolean = {
    //if (tokenIndex + 1 >= tokenArray.size) {
    if (isEOS) {
      return false
    } else {
      tokenIndex += 1
      if (isEOS)
        return false
      return true
    }
  }
  
  def advanceToEndOfBlock = {
    if (flagVerbose)
      println("Skipping to next block")
    var depth = 1
    while (depth > 0) {
      if (!advanceToken)
        depth = -1
      else if (currentToken.kind.name == 'lbracket)
        depth += 1
      else if (currentToken.kind.name == 'rbracket)
        depth -= 1
    }
    // Causes certain problems
    advanceToken
  }
    
  private def backtrack(x: Int) = {
    tokenIndex -= x
    if (tokenIndex < 0)
      tokenIndex = 0
  }

  private def currentToken(): Token = {
    if (tokenIndex + 1 > tokenArray.size)
      return new Token(Kind.eof, None)
    return tokenArray(tokenIndex)
  }

  /**
   * Construct a node using the productions as specified by its symbol.
   * The argument node should have its symbol set and have no children.
   *
   * Return a constructed node if successful, returns none if the Node
   * could not be constructed.
   */
  def constructNode(node: Node): Option[Node] = {
    val s = node.symbol
    if (flagVerbose)
      println("Attempting to construct node for " + s)
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
    var expected = ArrayBuffer.empty[(Symbol, Token)]
    prods.foreach((a: Array[Symbol]) => {
      var n = applyProduction(node, a, expected)
      if (n != None) {
        if (flagVerbose)
          println("Constructed node " + n.get.symbol)
        return n 
      }
    })

    if (!expected.isEmpty) {
      expected.map((t:(Symbol, Token)) => tokenError(t._1, t._2))
      advanceToEndOfBlock
      resetError
      return Some(new Node('Block, Array.empty[Node], None))
    }
    if (flagVerbose)
      println("Node construction failed for " + s)
    return None
  }

  /**
   * Apply the given productions on the given node. Return the constructed
   * node if succesful; otherwise, return None.
   */
  private def applyProduction(node: Node,
    a: Array[Symbol],
    expected: ArrayBuffer[(Symbol, Token)]): Option[Node] = {
      val i = a.iterator
      val children = ArrayBuffer.empty[Node]
      if (!i.hasNext) {
        if (flagVerbose)
          println("Matched epsilon (null string)")
        node.setChildren(Array(new Node('epsilon, Array.empty[Node])))
        return Some(node)
      }
      while (i.hasNext) {
        val symbol = i.next
        if (symbol == currentToken.kind.name) {
          if (flagVerbose)
            println("Matched " + symbol + " with token " + currentToken)
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
          var resultNode = constructNode(new Node(symbol, Array.empty[Node]))
          if (resultNode != None) {
            children += resultNode.get
            if (!i.hasNext) {
              node setChildren children.toArray
              //resetError
              return Some(node)
            }
          } else {
            if (!children.isEmpty && !errorState) {
              //if (isEOS)
              //  eosError(symbol)
              //else {
              expected += new Tuple2(symbol, currentToken)
              //}
              if (flagVerbose) {
                println("Destructing " + symbol + "; bactracking "
                  + children.map(_.getLength).reduce(_+_) + " tokens")
              }
              var consumedTokens = children.map(_.getLength).reduce(_+_)
              if (symbol == 'eop)
                backtrack(consumedTokens-1)
              else
                backtrack(consumedTokens)
            }
            // Deplete the iterator to exit the production (controlled by while i.hasNext)
            i.size
          }
        }
      }
      return None
  }

}
