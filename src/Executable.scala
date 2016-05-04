package compy

import scala.collection.mutable.StringBuilder
import scala.collection.mutable.HashMap

// Maybe include length-2 opcode validation?
class Executable {

  val opCodes = Array.fill[String](0x100)("00")
  def outOfMemory = ( ip + 1 > hp )
  def checkOutOfMemory = {
    if (outOfMemory)
      throw new Exception("The executabel image has run out of memory.")
  }
  var ip = 0x0
  var hp = 0xff
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
        case _ => ()
      }
    }
  }

  def addressStatic(): Unit = {
    for ( k <- staticTable.keys ) {
      staticTable.get(k).get.setAddress(ip)
      ip += 1
    }
  }

  def varDecl(se: SymbolEntry): Unit =  {
    //val oct = OCTemplate('VarDecl,
    //    id=Some((staticCounter+48).toChar) /*Fix*/)
    //insert(oct)
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

  def assignStatement(se: SymbolEntry, argString: String): Unit = {
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

  /**
   * Decide how to handle int + int as opposed to int + var
   */
  def intop(varArg: Int, se: SymbolEntry): Unit = {
    ???
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
