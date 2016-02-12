package compy

import scala.collection.Iterator


class Parser(val grammar: Grammar) {

  var tokenArray = Array[Token]()
  var tokens = Iterator[Token]()
  var currentToken: Token = _

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
    f('Program, 0)
  }

  def f(s: Symbol, level: Int): Boolean = {
    println("f with " + currentToken)
    //println(("\t" * level) + s)
    var rule = grammar.rules.getOrElse(s, Rule.empty)
    if (rule.name == 'empty) {
      //println("empty")
      // Return true because it is a non-terminating character?
      if (s == currentToken.kind.name) {
        println("~~" + s)
        return true
      }
      return false
    }

    val prods = rule.productions
    prods.foreach((a: Array[Symbol]) => {
      val i = a.iterator
      if (!i.hasNext) {
        //println("\t"*(level+1) + "'lam")
        // I think I should return true
        return true
      }
      while (i.hasNext) {
        val symbol = i.next
        //println(symbol)
        println(symbol + "\t" + currentToken.kind.name)
        if (symbol == currentToken.kind.name) {
          //println(("\t"*(level+1)) + symbol)
          // Handle end of token stream
          advanceToken
          if (!i.hasNext) {
            println("~~"+ a.mkString)
            return true
          }
          /* 
          if (i.hasNext) {
            f(i.next, tokens.next, level+1)
          } else {
            return true
          }
          */
        } else {
          if (f(symbol, level+1)) {
            println("~" + symbol)
            if (!i.hasNext) {
              return true
            }
          }
        }
      }
    })
    return false
  }
}
