package compy

import scala.collection.mutable._
import scala.util.matching.Regex

class Grammar {
  val kinds = MutableList.empty[Kind]
  val rules = HashMap.empty[Symbol,Rule]
  
  def addRule(name: Symbol, productions:Array[Array[Symbol]]) {
    rules += name -> new Rule(name, productions) 
  }

  def addKind(name: Symbol, regex: Regex) {
    kinds += new Kind(name, regex)
  }

}
