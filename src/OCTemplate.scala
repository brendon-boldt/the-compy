package compy

import scala.language.implicitConversions

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

object OCTemplate {

  var flagVerbose = false
  def vPrint(s: String): Unit = if (flagVerbose) println("OCTEMPLATE: " + s)

  /*
  def main(args: Array[String]) {
    println(OCTemplate('VarDecl))
    println(OCTemplate('LitAssign, id = Some('0'), lit=Some(31)))
    println(OCTemplate('PrintLit, id = Some('0')))
  }
  */

  implicit def booleanToOpCode(b: Boolean) = if (b) "01" else "00"

  /*
   * equ must be in the form "00" or "01"
   */
  def apply(op: Symbol,
    id: Option[Char] = None,
    id2: Option[Char] = None,
    lit: Option[String] = None,
    equ: Option[Boolean] = None,
    ptr: Option[Int] = None
    ) :OCTemplate = {
      vPrint("Generating OCT: " + op)
      op match {

    case 'LitAssign => {
      assert(!id.isEmpty,"id must be set for LitAssign")
      assert(!lit.isEmpty,"lit must be set for LitAssign")
      var litVal = lit.get
      if (litVal == "true")
        litVal = "1"
      else if (litVal == "false")
        litVal = "0"
      new OCTemplate(ArrayBuffer[String]
        ("A9", "%02X".format(litVal.toInt), "8D", "T"+id.get, "XX"))
    }

    case 'StringAssign => {
      assert(!id.isEmpty,"id must be set for StringAssign")
      assert(!ptr.isEmpty,"ptr must be set for StringAssign")
      new OCTemplate(ArrayBuffer[String]
        ("A9", "%02X".format(ptr.get), "8D", "T"+id.get, "XX"))
    }

    case 'PrintString => {
      assert(!id.isEmpty,"id must be set for PrintLit")
      new OCTemplate(ArrayBuffer[String]
        ("AC", "T"+id.get, "XX", "A2", "02", "FF"))
    }

    case 'PrintStringLit => {
      assert(!ptr.isEmpty,"ptr must be set for PrintStringLit")
      new OCTemplate(ArrayBuffer[String]
        ("A0", "%02X".format(ptr.get), "A2", "02", "FF"))
    }

    case 'PrintLit => {
      assert(!id.isEmpty,"id must be set for PrintLit")
      new OCTemplate(ArrayBuffer[String]
        ("AC", "T"+id.get, "XX", "A2", "01", "FF"))
    }

    case 'PrintAcc => {
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "AC", "MM", "XX", "A2", "01", "FF"))
    }

    case 'AccAssign => {
      assert(!id.isEmpty,"id must be set for AccAssign")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "T"+id.get, "XX"))
    }

    case 'AddInt => {
      assert(!lit.isEmpty,"lit must be set for AddInt")
      assert(!id.isEmpty,"id must be set for AddInt")
      new OCTemplate(ArrayBuffer[String]
        ("A9", "%02X".format(lit.get.toInt), "6D", "T"+id.get, "XX"))
    }

    // Here
    case 'CompareLitVar => {
      assert(!lit.isEmpty,"lit must be set for CompareLit")
      assert(!id.isEmpty,"id must be set for CompareLit")
      new OCTemplate(ArrayBuffer[String]
        ("A2", "%02X".format(lit.get.toInt), "EC", "T"+id.get, "XX"))
    }

    // Here
    case 'CompareVarVar => {
      assert(!id.isEmpty,"id must be set for CompareLitVar")
      assert(!id2.isEmpty,"id2 must be set for CompareLitVar")
      new OCTemplate(ArrayBuffer[String]
        ("AE", "T"+id.get, "XX", "EC", "T"+id2.get, "XX"))
    }

    // Here
    case 'CompareVarAcc => {
      assert(!id.isEmpty,"id must be set for CompareLitAcc")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "AE", "MM", "XX" , "EC", "T"+id.get, "XX"))
    }

    // Here
    case 'CompareLitAcc => {
      assert(!lit.isEmpty,"id must be set for CompareLitAcc")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "A2", "%02X".format(lit.get.toInt), "EC", "MM", "XX"))
    }

    case 'AccToM => {
      assert(!id.isEmpty,"id must be set for AccToM")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "M"+id.get, "XX"))
    }

    // Here
    case 'CompareMAcc => {
      assert(!id.isEmpty,"id must be set for CompareMAcc")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "AE", "MM", "XX", "EC", "M"+id.get, "XX"))
    }

    case 'ZFToAcc => {
      assert(!equ.isEmpty,"equ must be set for ZFToAcc")
      new OCTemplate(ArrayBuffer[String]
      ("A9", !equ.get+"", "D0", "02", "A9", equ.get+""))
    }

    case 'CompareString => {
      ???
      new OCTemplate(ArrayBuffer.empty[String])
    }

    case 'IfStatement => {
      assert(!id.isEmpty,"id must be set for IfStatement")
      assert(!equ.isEmpty,"equ must be set for IfStatement")
      if (equ.get)
        new OCTemplate(ArrayBuffer[String]
          ("D0", "J"+id.get))
      else
        new OCTemplate(ArrayBuffer[String]
          ("A9", "00", "8D", "MM", "XX", "AE", "MM", "XX",
            "D0", "03", "EE", "MM", "XX",
            "EC", "MM", "XX", "D0", "J"+id.get))
    }

    case 'PostWhileStatement => {
      assert(!id.isEmpty,"id must be set for PostWhileStatement")
      new OCTemplate(ArrayBuffer[String]
        ("AE", "MM", "XX", "EE", "MM", "XX",
         "EC", "MM", "XX", "D0", "J"+id.get))
    }

    case 'HALT => {
      new OCTemplate(ArrayBuffer[String]
        ("00"))
    }

    case _ => {
      throw new Exception("OpCode Template: "+ op + " not found")
      emptyTemplate
    }
  }
  }
     
  val emptyTemplate = new OCTemplate(ArrayBuffer.empty[String])
}

// Should it be a String or a Symbol?
class OCTemplate(val opCodes: ArrayBuffer[String]) {

  def apply(i: Int) = opCodes(i)

  def length = opCodes.length

  override def toString(): String = {
    opCodes.mkString(" ")
  }
}
