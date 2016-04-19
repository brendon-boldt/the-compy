package compy

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer


class Parser(val grammar: Grammar) {

  var flagVerbose = false
  def vPrint(s: String) = {
    if (flagVerbose)
      println("PARSER: " + s)
  }

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
    resetError
    //rootNode = new Node('Program, Array.empty[Node])
    var emptyChild = new Node('PLACEHOLDER, Array.empty[Node])
    rootNode = new Node('Program, Array[Node](emptyChild))

    val startIndex = tokenIndex
    var res = constructNode(rootNode)
    if (rootNode.children.size > 0
        && rootNode.children(0).symbol == 'PLACEHOLDER
        && tokenIndex == startIndex) {
      tokenError('lbracket, currentToken)
    }
    errorState = !errors.isEmpty
    errorState = errorState || (res == None)
    // Not sure why this line is hear
    //errorState = errorState || (!eosErrorSymbol.isEmpty)
    makeErrorString

    if (errorState) {
      println(errorString.stripLineEnd)
    } else if (tokenIndex < tokenArray.length) {
      errorState = true
      println("Parse Error: Expecting end of program; got "
        + tokenArray(tokenIndex).string + " at line " + tokenArray(tokenIndex).line)
    }
  }
  
  private def advanceToNextProgram = {
      do {
        advanceToken
      } while (!isEOS && currentToken.kind.name != 'eop)
      advanceToken
      //println("Advanced to: " + currentToken)
  }

  private def makeErrorString = {
    errors.foreach((arg: (Symbol, Token)) => arg match {
      case (s,t) => errorString += ("Parse Error: expecting " + Grammar.getLiteral(s) +
        " got " + t.string + " on line " + t.line + "\n")
    })
    if (!eosErrorSymbol.isEmpty)
      errorString += ("Parse Error: expecting " + Grammar.getLiteral(eosErrorSymbol.get) +
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
    errorString = ""
    errors.reduceToSize(0)
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
    vPrint("Skipping to next block")
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
    vPrint("Attempting to construct node for " + s)
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
      val n = applyProduction(node, a, expected)
      if (!n.isEmpty) {
        vPrint("Constructed node " + n.get.symbol)
        return n 
      }
    })

    if (!expected.isEmpty) {
      expected.map((t:(Symbol, Token)) => tokenError(t._1, t._2))
    }
    vPrint("Node construction failed for " + s)
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
        vPrint("Matched epsilon (null string)")
        node.setChildren(Array(new Node('epsilon, Array.empty[Node], parent = Some(node))))
        return Some(node)
      }
      while (i.hasNext) {
        val symbol = i.next
        if (symbol == currentToken.kind.name) {
          vPrint("Matched " + symbol + " with token " + currentToken)
          children += new Node(symbol, Array.empty[Node], parent = Some(node))
          children.last.token = Some(currentToken)
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
          var resultNode = constructNode(new Node(symbol, Array.empty[Node], parent = Some(node)))
          if (resultNode != None) {
            children += resultNode.get
            if (!i.hasNext) {
              node setChildren children.toArray
              return Some(node)
            }
          } else {
            //if (!grammar.isTerminal(symbol) && !errorState) {
            if (!children.isEmpty && !errorState) {
              //if (isEOS)
              //  eosError(symbol)
              //else {
              expected += new Tuple2(symbol, currentToken)
              //}
              if (!children.isEmpty) {
                vPrint("Destructing " + symbol + "; bactracking "
                    + children.map(_.getLength).reduce(_+_) + " tokens")
                var consumedTokens = children.map(_.getLength).reduce(_+_)
                if (symbol == 'eop)
                  backtrack(consumedTokens-1)
                else
                  backtrack(consumedTokens)
              }
            }
            // Deplete the iterator to exit the production (controlled by while i.hasNext)
            i.size
          }
        }
      }
      return None
  }

}
