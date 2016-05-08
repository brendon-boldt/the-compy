package compy

import scala.language.implicitConversions

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

object OCTemplate {

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
      println("Generating OCT: " + op)
      op match {

    case 'LitAssign => {
      if (id.isEmpty) throw new Exception("id must be set for LitAssign")
      if (lit.isEmpty) throw new Exception("lit must be set for LitAssign")
      var litVal = lit.get
      if (litVal == "true")
        litVal = "1"
      else if (litVal == "false")
        litVal = "0"
      new OCTemplate(ArrayBuffer[String]
        ("A9", "%02X".format(litVal.toInt), "8D", "T"+id.get, "XX"))
    }

    case 'StringAssign => {
      if (id.isEmpty) throw new Exception("id must be set for StringAssign")
      if (ptr.isEmpty) throw new Exception("ptr must be set for StringAssign")
      new OCTemplate(ArrayBuffer[String]
        ("A9", "%02X".format(ptr.get), "8D", "T"+id.get, "XX"))
    }

    case 'PrintString => {
      if (id.isEmpty) throw new Exception("id must be set for PrintLit")
      new OCTemplate(ArrayBuffer[String]
        ("AC", "T"+id.get, "XX", "A2", "02", "FF"))
    }

    case 'PrintStringLit => {
      if (ptr.isEmpty) throw new Exception("ptr must be set for PrintStringLit")
      new OCTemplate(ArrayBuffer[String]
        ("A0", "%02X".format(ptr.get), "A2", "02", "FF"))
    }

    case 'PrintLit => {
      if (id.isEmpty) throw new Exception("id must be set for PrintLit")
      new OCTemplate(ArrayBuffer[String]
        ("AC", "T"+id.get, "XX", "A2", "01", "FF"))
    }

    case 'PrintAcc => {
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "AC", "MM", "XX", "A2", "01", "FF"))
    }

    case 'AccAssign => {
      if (id.isEmpty) throw new Exception("id must be set for AccAssign")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "T"+id.get, "XX"))
    }

    case 'AddInt => {
      if (lit.isEmpty) throw new Exception("lit must be set for AddInt")
      if (id.isEmpty) throw new Exception("id must be set for AddInt")
      new OCTemplate(ArrayBuffer[String]
        ("A9", "%02X".format(lit.get.toInt), "6D", "T"+id.get, "XX"))
    }

    case 'CompareLitVar => {
      if (lit.isEmpty) throw new Exception("lit must be set for CompareLit")
      if (id.isEmpty) throw new Exception("id must be set for CompareLit")
      if (equ.isEmpty) throw new Exception("equ must be set for CompareLit")
      new OCTemplate(ArrayBuffer[String]
        ("A2", "%02X".format(lit.get.toInt), "EC", "T"+id.get, "XX",
         "A9", !equ.get+"", "D0", "02", "A9", equ.get+""))
    }

    case 'CompareVarVar => {
      if (id.isEmpty) throw new Exception("id must be set for CompareLitVar")
      if (id2.isEmpty) throw new Exception("id2 must be set for CompareLitVar")
      if (equ.isEmpty) throw new Exception("equ must be set for CompareLitVar")
      new OCTemplate(ArrayBuffer[String]
        ("AE", "T"+id.get, "XX", "EC", "T"+id2.get, "XX",
         "A9", !equ.get+"", "D0", "02", "A9", equ.get+""))
    }

    case 'CompareVarAcc => {
      if (id.isEmpty) throw new Exception("id must be set for CompareLitAcc")
      if (equ.isEmpty) throw new Exception("equ must be set for CompareLitAcc")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "AE", "MM", "XX" , "EC", "T"+id.get, "XX",
         "A9", !equ.get+"", "D0", "02", "A9", equ.get+""))
    }

    case 'CompareLitAcc => {
      if (lit.isEmpty) throw new Exception("id must be set for CompareLitAcc")
      if (equ.isEmpty) throw new Exception("equ must be set for CompareLitAcc")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "A2", "%02X".format(lit.get.toInt), "EC", "MM", "XX",
         "A9", !equ.get+"", "D0", "02", "A9", equ.get+""))
    }

    case 'AccToM => {
      if (id.isEmpty) throw new Exception("id must be set for AccToM")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "M"+id.get, "XX"))
    }

    case 'CompareMAcc => {
      if (equ.isEmpty) throw new Exception("equ must be set for CompareMAcc")
      if (id.isEmpty) throw new Exception("id must be set for CompareMAcc")
      new OCTemplate(ArrayBuffer[String]
        ("8D", "MM", "XX", "AE", "MM", "XX", "EC", "M"+id.get, "XX",
         "A9", !equ.get+"", "D0", "02", "A9", equ.get+""))
    }

    case 'ZFToAcc => {
      if (equ.isEmpty) throw new Exception("equ must be set for ZFToAcc")
    }

    /*
    case 'CompareString => {

    }
    */

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
