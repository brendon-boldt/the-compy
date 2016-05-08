package compy

import scala.collection.mutable.StringBuilder
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
//import scala.collection.mutable.Set

// Maybe include length-2 opcode validation?
class Executable {

  val opCodes = Array.fill[String](0x100)("00")
  def outOfMemory = ( ip + 1 > hp )
  def checkOutOfMemory = {
    if (outOfMemory)
      throw new Exception("The executable image has run out of memory.")
  }
  var ip = 0x0
  var hp = 0xff
  var mm = 0x0

  var storeZF = true

  // This is needed if the value from the accumulator needs to be stored
  // to the X register for future comparison.
  //var accToMMap = HashMap.empty[Node,Int] 

  // SE is a unique ID for vars; String is its name for BP; Int is its addr 
  val staticTable = HashMap.empty[SymbolEntry, StaticEntry]
  val memTable = HashMap.empty[Node, StaticEntry]
  val jumpTable = HashMap.empty[Node, StaticEntry]
  val jumpStack = Stack.empty[Int]
  var staticCounter = 0
  var memCounter = 0
  var jumpCounter = 0

  def getChar(arg: Int): Char = (arg + 48).toChar

  def pushIP = jumpStack push ip

  def getStaticAddress(c: Char): String =
    staticTable.find( (arg:(SymbolEntry, StaticEntry))
        => (c == arg._2.id)).get._2.getAddressString

  def getMemAddress(c: Char): String =
    memTable.find( (arg:(Node, StaticEntry))
        => (c == arg._2.id)).get._2.getAddressString

  def getJumpAddress(c: Char): String =
    jumpTable.find( (arg:(Node, StaticEntry))
        => (c == arg._2.id)).get._2.getAddressString
  
  def calculateJump(node: Node): Int = {
    val jump = ip - jumpStack.pop
    println("Jump: " + "%02X".format(jump))
    jump
  }

  def storeJump(node: Node, jump: Int): Unit = {
    jumpTable.get(node).get.setAddress(jump)
  }



  def backpatch(): Unit = {
    for ( i <- Range(0, opCodes.length) ) {
      if (opCodes(i)(0) == 'T') {
        opCodes(i) = getStaticAddress(opCodes(i)(1))
      } else if (opCodes(i)(0) == 'M' && opCodes(i)(1) != 'M') {
        opCodes(i) = getMemAddress(opCodes(i)(1))
      } else if (opCodes(i)(0) == 'J') {
        opCodes(i) = getJumpAddress(opCodes(i)(1))
      } else opCodes(i) match {
        case "XX" => opCodes(i) = "00"
        case "MM" => opCodes(i) = "%02X" format mm
        case _ => ()
      }
    }
  }

  def addressStatic(): Unit = {
    for ( k <- staticTable.keys ) {
      staticTable.get(k).get.setAddress(ip)
      ip += 1
    }
    for ( k <- memTable.keys ) {
      memTable.get(k).get.setAddress(ip)
      ip += 1
    }
    mm = ip
    ip += 1
  }

  def varDecl(se: SymbolEntry): Unit =  {
    staticTable += se -> new StaticEntry(getChar(staticCounter))
    staticCounter += 1
  }

  def createString(s: String): Int = {
    val chars = s.slice(1,s.length-1).map(_.toInt)
    hp -= chars.length
    for ( i <- Range(0, chars.length) ) {
      opCodes(hp + i) = "%02X".format(chars(i))
    }
    hp -= 1
    return hp + 1
  }

  /**
   * Assigns a literal value
   */
  def litAssign(se: SymbolEntry, argString: String): Unit = {
    if (se.varType == 'string) {
      var ptr = createString(argString)
      checkOutOfMemory
      val oct = OCTemplate('StringAssign,
          id=Some(staticTable.get(se).get.id),
          ptr=Some(ptr))
      insert(oct)
    } else {
      val oct = OCTemplate('LitAssign,
          id=Some(staticTable.get(se).get.id),
          lit=Some(argString))
      insert(oct)
    }
  }

  def accAssign(se: SymbolEntry): Unit = {
    val oct = OCTemplate('AccAssign,
      id=Some(staticTable.get(se).get.id))
    insert(oct)
  }

  def intop(intArg: String, se: SymbolEntry): Unit = {
    val oct = OCTemplate('AddInt,
      lit=Some(intArg),
      id=Some(staticTable.get(se).get.id))
    insert(oct)
  }

  def compareLitVar(equ: Boolean, se: SymbolEntry, string: String): Unit = {
    var value = string
    if (string == "true")
      value = "1"
    else if (string == "false")
      value = "0"
    val oct = OCTemplate('CompareLitVar,
      lit=Some(value),
      id=Some(staticTable.get(se).get.id))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def compareVarVar(equ: Boolean, se1: SymbolEntry, se2: SymbolEntry): Unit = {
    val oct = OCTemplate('CompareVarVar,
      id=Some(staticTable.get(se1).get.id),
      id2=Some(staticTable.get(se2).get.id))
    insert(oct)
    insert(OCTemplate('ZFToAcc, equ=Some(equ)))
  }

  def compareVarAcc(equ: Boolean, se: SymbolEntry): Unit = {
    val oct = OCTemplate('CompareVarAcc,
      id=Some(staticTable.get(se).get.id))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def compareLitAcc(equ: Boolean, string: String): Unit = {
    var value = string
    if (string == "true")
      value = "1"
    else if (string == "false")
      value = "0"
    val oct = OCTemplate('CompareLitAcc,
      lit=Some(value))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def storeAccToM(node: Node): Unit = {
    insert(OCTemplate('AccToM, id=Some(memTable.get(node).get.id)))
  }

  def compareAccAcc(equ: Boolean, node: Node): Unit = {
    val oct = OCTemplate('CompareMAcc,
      id=Some(memTable.get(node).get.id))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def compareString(equ: Boolean, se: SymbolEntry, value: String): Unit = {
    ???
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def ifStatement(node: Node, equ: Boolean): Unit = {
    //jumpStack push node
    val char = getChar(jumpCounter)
    jumpTable += node -> new StaticEntry(char)
    jumpCounter += 1
    insert(OCTemplate('IfStatement,
      equ=Some(equ),
      id=Some(char)))
    jumpStack push ip
  }


  def printString(se: SymbolEntry): Unit = {
    val oct = OCTemplate('PrintString,
      id=Some(staticTable.get(se).get.id))
    insert(oct)
  }

  def printStringLit(argString: String): Unit = {
    val ptr = createString(argString)
    val oct = OCTemplate('PrintStringLit,
      ptr=Some(ptr))
    insert(oct)
  }

  def printLit(se: SymbolEntry): Unit = {
    val oct = OCTemplate('PrintLit,
      id=Some(staticTable.get(se).get.id))
    insert(oct)
  }

 def printAcc(): Unit = {
    insert(OCTemplate('PrintAcc))
 }

  def insert(oct: OCTemplate) { 
    if (insertAt(oct, ip))
      ip += oct.length
  }

  private def insertAt(oct: OCTemplate, address: Int): Boolean =  { 
    if (address < 0 || (address + oct.length - 1) > 0xff)
      throw new Exception("OCTemplate must fit in the range [0,0xff]")
    for ( i <- Range(0, oct.length) ) {
      opCodes(address+i) = oct(i)
    }
    return true
  }

  def apply = opCodes(_)

  override def toString(): String = {
    val string = new StringBuilder
    for ( i <- Range(0, opCodes.length) ) {
      string append (opCodes(i) + " ")
      if ((i+1) % 8 == 0 )
        string += '\n'
    }
    return string.toString.stripLineEnd
  }
}
