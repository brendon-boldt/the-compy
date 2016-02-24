package compy

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer


class Parser(val grammar: Grammar) {

  var flagVerbose = false

  var tokenArray = Array[Token]()
  var tokenIndex = 0
  var rootNode = new Node('empty, Array.empty[Node])
  var error = false
  var errorString = ""

  /**
   * Reset error state to false.
   * Invoked it one production fails and subsequent succeeds.
   */
  def resetError = {
    error = false
    errorString = ""
  }

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
    error = false
    // Initialize the root node with no children (it'll be happier that way)
    rootNode = new Node('Program, Array.empty[Node])
    // Check whether the whole parse succeeded or not
    if (constructNode(rootNode) == None) {
      if (flagVerbose)
        println("Parse failed; advancing to next program")
      do {
        // Since the parse failed for this program, advance the token stream
        // to the next program.
        advanceToken
      } while (!isEOS && currentToken.kind.name != 'eop)
      advanceToken
      // Print the errors which occurred 
      println(errorString.stripLineEnd)
    } else {
      if (rootNode.children.length == 1)
        println("Parser warning: forgotten '$' at end of program")
      resetError
    }
  }

  /**
   * An error generator when an expected token is not found.
   */
  private def tokenError(symbol: Symbol) =  {
    error = true
    errorString += ("Parse Error: expecting " + Grammar.getLiteral(symbol) + " got "
                  + currentToken.string + " on line " + currentToken.line + "\n")
  }

  /**
   * For when I do not want explain what actatually went wrong.
   */
  private def genericError = {
    error = true
    errorString += "Parse Error: an error occured while parsing\n"
  }

  /**
   * End of stream error -- up the creek without a paddle
   */
  private def eosError(symbol: Symbol) = {
    error = true
    errorString = "Parse Error: end of token steam reached; expecting " + 
      Grammar.getLiteral(symbol) + "\n"
  }

  def isEOS(): Boolean = (tokenIndex + 1 >= tokenArray.size)

  /**
   * Advance the token iterator. Return whether it was succesfully advanced.
   */
  private def advanceToken(): Boolean = {
    if (tokenIndex + 1 >= tokenArray.size)
      return false
    else {
      tokenIndex += 1
      return true
    }
  }
  
  private def backtrack(x: Int) = {
    tokenIndex -= x
    if (tokenIndex < 0)
      tokenIndex = 0
  }

  private def currentToken(): Token = return tokenArray(tokenIndex)

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
    // Get the productions associated with the node's symbol
    var rule = grammar.rules.getOrElse(s, Rule.empty)
    if (rule.name == 'empty) {
      // If the current node is terminal, that is, has no further productions,
      // check if the current token is in fact the node we are on. Otherwise,
      // return None.
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
        if (flagVerbose)
          println("Constructed node " + n.get.symbol)
        return n 
      }
    })
    if (flagVerbose)
      println("Node construction failed for " + s)
    return None
  }

  /**
   * Apply the given productions on the given node. Return the constructed
   * node if succesful; otherwise, return None.
   */
  private def applyProduction(node: Node, a: Array[Symbol]): Option[Node] = {
      val i = a.iterator
      // Create an empty mutable array to store the children generated fromm
      // the production.
      val children = ArrayBuffer.empty[Node]
      // If the production is simply an epsilon transition
      if (!i.hasNext) {
        if (flagVerbose)
          println("Matched epsilon (null string)")
        node.setChildren(Array(new Node('epsilon, Array.empty[Node])))
        return Some(node)
      }
      // Loop through the production elements
      while (i.hasNext) {
        val symbol = i.next
        // If the current token matches the symbol in the production
        if (symbol == currentToken.kind.name) {
          if (flagVerbose)
            println("Matched " + symbol + " with token " + currentToken)
          // Add the newly constructed node to the child list
          children += new Node(symbol, Array.empty[Node])
          children.last.value = Some(currentToken)
          // If the end of the production has not been reached
          if (i.hasNext) {
            if (!advanceToken) {
              // If the end of the token stream has been reached, produce an error
              eosError(i.next)
              return None
            }
          // If the end of the production has been reached, return argument node
          // with the generated children
          } else {
            advanceToken
            node setChildren children.toArray
            return Some(node)
          }
        // If the symbol does not match the current token
        } else {
          // The part you've been waiting for: recursion!
          // Attempt to construct a new node with the unmatched symbol.
          // If the symbol is terminal, this will return none; if it is a nonterminal
          // Start the process above over again.
          var resultNode = constructNode(new Node(symbol, Array.empty[Node]))
          // If the nonterminal symbol matched (that is, is not None)
          if (resultNode != None) {
            // Append the newly constructed node to the children
            children += resultNode.get
            // If the end of the production has been reached
            if (!i.hasNext) {
              node setChildren children.toArray
              // Erase any previous errors; this will need to change if I want to do
              // multiple error detection/recovery.
              resetError
              return Some(node)
            }
          // If the recursion call returned None for the node
          } else {
            if (!children.isEmpty && !error) {
              // If a production fails, it's an error; either because of end of stream
              // or by a wrong token/symbol. Note that this error can be erased if a
              // subsequent production for the same rule succeeds.
              if (isEOS)
                eosError(symbol)
              else
                tokenError(symbol)
              // Maybe add error recovery by jumping to the next block
              if (flagVerbose) {
                println("Destructing " + symbol + "; bactracking "
                  + children.map(_.getLength).reduce(_+_) + " tokens")
              }
              // Since my parser is not predictive, I use backtracking to handle
              // common prefixes. If a production matched some symbols/tokens but
              // consequently failed, backtrack however many tokens were consumed
              // and start matching the next production.
              var consumedTokens = children.map(_.getLength).reduce(_+_)
              // If the '$' was forgotten at the end of the program; we need to
              // go back one fewer token than normal.
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
