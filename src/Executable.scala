package compy

import scala.collection.mutable.StringBuilder
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

class Executable {

  var flagVerbose = false
  def vPrint(s: String): Unit = if(flagVerbose) println("EXECUTABLE: " + s)

  val opCodes = Array.fill[String](0x100)("00")
  def outOfMemory = ( ip + 1 > hp )
  def checkOutOfMemory = {
    if (outOfMemory)
      throw new Exception("The executable image has run out of memory.")
  }
  // Instruction Pointer, that is
  private var ip = 0x0
  private var hp = 0xff
  // MM is just multi-use memory for whenever I just need to dump something
  private var mm = 0x0
  def getIP = ip
  var storeZF = true


  def getChar(arg: Int): Char = (arg + 48).toChar

  // Here are all of the super-fun memory structures
  val stringMap = HashMap.empty[String, Int]
  val staticTable = HashMap.empty[SymbolEntry, StaticEntry]
  val memTable = HashMap.empty[Node, StaticEntry]
  val jumpTable = HashMap.empty[Node, StaticEntry]
  val jumpStack = Stack.empty[Int]
  var staticCounter = 0
  var memCounter = 0
  var jumpCounter = 0

  /**
   * There are many ways one can interpret a literal (as opposed to a
   * figurative), and this will do it for you.
   */
  def translateLit(s: String): String = {
    if (s == "true")
      return "1"
    else if (s == "false")
      return "0"
    else if (s.forall(_.isDigit))
      return s
    else
      return stringMap(s.slice(1,s.length-1)).toString
  }

  def pushIP = jumpStack push ip
  def popIP = jumpStack.pop

  // This is how backpatching finds the correct address to substitute
  def getStaticAddress(c: Char): String =
    staticTable.find( (arg:(SymbolEntry, StaticEntry))
        => (c == arg._2.id)).get._2.getAddressString

  def getMemAddress(c: Char): String =
    memTable.find( (arg:(Node, StaticEntry))
        => (c == arg._2.id)).get._2.getAddressString

  def getJumpAddress(c: Char): String =
    jumpTable.find( (arg:(Node, StaticEntry))
        => (c == arg._2.id)).get._2.getAddressString
  
  /**
   * *BIG FLASHING RED LIGHTS*
   * This function causes statechange from popping a value
   * off of the stack.
   */
  def calculateJump(): Int = {
    val jump = ip - jumpStack.pop
    vPrint("Calculated jump: " + "%02X".format(jump))
    jump
  }

  def storeJump(node: Node, jump: Int): Unit = {
    vPrint("Storing jump for: " + node.symbol)
    jumpTable(node).setAddress(jump)
  }

  /**
   * Pretty self-explanatory
   */
  def backpatch(): Unit = {
    for ( i <- Range(0, opCodes.length) ) {
      if (opCodes(i)(0) == 'T') {
        vPrint("Replacing " + opCodes(i))
        opCodes(i) = getStaticAddress(opCodes(i)(1))
      } else if (opCodes(i)(0) == 'M' && opCodes(i)(1) != 'M') {
        vPrint("Replacing " + opCodes(i))
        opCodes(i) = getMemAddress(opCodes(i)(1))
      } else if (opCodes(i)(0) == 'J') {
        vPrint("Replacing " + opCodes(i))
        opCodes(i) = getJumpAddress(opCodes(i)(1))
      } else opCodes(i) match {
        case "XX" => opCodes(i) = "00"
        case "MM" => opCodes(i) = "%02X" format mm
        case _ => ()
      }
    }
  }

  /**
   * Assign addresses to all of the static/multi-memory vairables
   */
  def addressStatic(): Unit = {
    for ( k <- staticTable.keys ) {
      vPrint("Addressing " + k + " to " + ip )
      staticTable(k).setAddress(ip)
      ip += 1
    }
    for ( k <- memTable.keys ) {
      vPrint("Addressing " + k + " to " + ip )
      memTable(k).setAddress(ip)
      ip += 1
    }
    mm = ip
    ip += 1
  }

  def varDecl(se: SymbolEntry): Unit =  {
    vPrint("Declaring variable " + se)
    staticTable += se -> new StaticEntry(getChar(staticCounter))
    staticCounter += 1
  }

  /**
   * Check if there is already a representation of that string present,
   * if not, create a new one in memory and return the pointer
   */
  def createString(argString: String): Int = {
    val string = argString.slice(1,argString.length-1)
    if (stringMap.contains(string)) {
      vPrint("Retrieved string: " + string)
      return stringMap(string)
    } else {
      vPrint("Creating new string: " + string)
      val chars = string.map(_.toInt)
      hp -= chars.length
      for ( i <- Range(0, chars.length) ) {
        opCodes(hp + i) = "%02X".format(chars(i))
      }
      hp -= 1
      stringMap += string -> (hp + 1)
      return hp + 1
    }
  }

  /**
   * Assigns a literal value
   */
  def litAssign(se: SymbolEntry, argString: String): Unit = {
    if (se.varType == 'string) {
      var ptr = createString(argString)
      checkOutOfMemory
      val oct = OCTemplate('StringAssign,
          id=Some(staticTable(se).id),
          ptr=Some(ptr))
      insert(oct)
    } else {
      val oct = OCTemplate('LitAssign,
          id=Some(staticTable(se).id),
          lit=Some(argString))
      insert(oct)
    }
  }

  def accAssign(se: SymbolEntry): Unit = {
    val oct = OCTemplate('AccAssign,
      id=Some(staticTable(se).id))
    insert(oct)
  }

  def intop(intArg: String, se: SymbolEntry): Unit = {
    val oct = OCTemplate('AddInt,
      lit=Some(intArg),
      id=Some(staticTable(se).id))
    insert(oct)
  }
  
  /**
   * I, unforutnately, need one of these method for whatever combination
   * of literals, variables, and expressions might get compared.
   */
  def compareLitVar(equ: Boolean, se: SymbolEntry, string: String): Unit = {
    var value = translateLit(string)
    val oct = OCTemplate('CompareLitVar,
      lit=Some(value),
      id=Some(staticTable(se).id))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def compareVarVar(equ: Boolean, se1: SymbolEntry, se2: SymbolEntry): Unit = {
    val oct = OCTemplate('CompareVarVar,
      id=Some(staticTable(se1).id),
      id2=Some(staticTable(se2).id))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def compareVarAcc(equ: Boolean, se: SymbolEntry): Unit = {
    val oct = OCTemplate('CompareVarAcc,
      id=Some(staticTable(se).id))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def compareLitAcc(equ: Boolean, string: String): Unit = {
    var value = translateLit(string)
    val oct = OCTemplate('CompareLitAcc,
      lit=Some(value))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def storeAccToM(node: Node): Unit = {
    insert(OCTemplate('AccToM, id=Some(memTable(node).id)))
  }

  /**
   * This probably should be compareExprExpr, but vim's refactoring is not
   * the greatest, so I'll save that for another time.
   */
  def compareAccAcc(equ: Boolean, node: Node): Unit = {
    val oct = OCTemplate('CompareMAcc,
      id=Some(memTable(node).id))
    insert(oct)
    if (storeZF)
      insert(OCTemplate('ZFToAcc, equ=Some(equ)))
    else
      storeZF = true
  }

  def ifStatement(node: Node, equ: Boolean): Unit = {
    val char = getChar(jumpCounter)
    jumpTable += node.children(0) -> new StaticEntry(char)
    jumpCounter += 1
    insert(OCTemplate('IfStatement,
      equ=Some(equ),
      id=Some(char)))
    jumpStack push ip
  }

  def whileStatement(node: Node, equ: Boolean): Unit = {
    val char = getChar(jumpCounter)
    jumpTable += node -> new StaticEntry(char)
    jumpCounter += 1
    ifStatement(node, equ)
  }

  def postWhileStatement(node: Node): Unit = {
    val id = jumpTable(node).id
    insert(OCTemplate('PostWhileStatement,
      id=Some(id)))
  }

  def printString(se: SymbolEntry): Unit = {
    val oct = OCTemplate('PrintString,
      id=Some(staticTable(se).id))
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
      id=Some(staticTable(se).id))
    insert(oct)
  }

 def printAcc(): Unit = {
    insert(OCTemplate('PrintAcc))
 }

  /**
   * The method to neatly store an opcode at IP
   */
  def insert(oct: OCTemplate) { 
    vPrint("Inserting OpCode " + oct)
    if (insertAt(oct, ip))
      ip += oct.length
  }

  private def insertAt(oct: OCTemplate, address: Int): Boolean =  { 
    if (address < 0 || (address + oct.length - 1) > 0xff)
      throw new Exception("Ran out of memory; OCTemplate must fit in the range [0,0xff]")
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
