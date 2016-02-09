package compy

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class Grammar {
  //val kinds = HashMap[Pattern,Symbol]()
  val patternMap = HashMap.empty[Symbol,ArrayBuffer[Kind]]
  val patterns = ArrayBuffer.empty[Pattern]

  def addKind(patternName: Symbol, name: Symbol, regex: Regex) {
    //patternMap(
  }

  def addPattern(name: Symbol, regex: Regex) {
    //patterns += 
    patternMap += new Pattern(name, regex) -> ArrayBuffer.empty[Kind]
  }

}
