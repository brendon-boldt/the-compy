package compy

import scala.collection.mutable._
import scala.util.matching.Regex


object Grammar {

  def getLiteral(s: Any): String = s match {
    case 'rparen => ")"
    case 'lparen => "("
    case 'rbracket => "}"
    case 'lbracket => "{"
    case 'intop => "+"
    case 'assign => "="
    case 'eq => "=="
    case 'neq => "!="
    case 'eop => "$"
    case 'Expr => "expression"
    case 'Program => "program"
    case 'Block => "block"
    case _ => s.toString
  }

}


class Grammar {
  val kinds = MutableList.empty[Kind]
  val rules = HashMap.empty[Symbol,Rule]
  
  def addRule(name: Symbol, productions:Array[Array[Symbol]]) {
    rules += name -> new Rule(name, productions) 
  }

  def addKind(name: Symbol, regex: Regex) {
    kinds += new Kind(name, Some(regex))
  }
}
