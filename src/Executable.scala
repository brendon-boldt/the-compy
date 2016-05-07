package compy

import scala.collection.mutable.StringBuilder
import scala.collection.mutable.HashMap

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
  // Create a jump table

  // SE is a unique ID for vars; String is its name for BP; Int is its addr 
  val staticTable = HashMap.empty[SymbolEntry, StaticEntry]
  var staticCounter = 0

  def getStaticAddress(c: Char): String =
    staticTable.find( (arg:(SymbolEntry, StaticEntry))
        => (c == arg._2.id)).get._2.getAddressString
  

  def backpatch(): Unit = {
    for ( i <- Range(0, opCodes.length) ) {
      if (opCodes(i)(0) == 'T') {
        opCodes(i) = getStaticAddress(opCodes(i)(1))
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
    mm = ip
  }

  def varDecl(se: SymbolEntry): Unit =  {
    staticTable += se -> new StaticEntry((staticCounter+48).toChar/*Fix*/)
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
    val oct = OCTemplate('CompareLit,
      lit=Some(value),
      equ=Some(equ),
      id=Some(staticTable.get(se).get.id))
    insert(oct)
  }

  def compareVarVar(equ: Boolean, se1: SymbolEntry, se2: SymbolEntry): Unit = {
    val oct = OCTemplate('CompareVarVar,
      equ=Some(equ),
      id=Some(staticTable.get(se1).get.id),
      id2=Some(staticTable.get(se2).get.id))
    insert(oct)
  }

  def compareVarAcc(equ: Boolean, se: SymbolEntry): Unit = {
    val oct = OCTemplate('CompareVarAcc,
      equ=Some(equ),
      id=Some(staticTable.get(se).get.id))
    insert(oct)
  }

  def compareString(eq: Boolean, se: SymbolEntry, value: String): Unit = {

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
