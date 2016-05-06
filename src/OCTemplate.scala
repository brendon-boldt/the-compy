package compy

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

  def apply(op: Symbol,
    id: Option[Char] = None,
    lit: Option[String] = None,
    ptr: Option[Int] = None
    //lit: Option[Int] = None
    ) :OCTemplate = op match {
    case 'VarDecl => {
      if (id.isEmpty) throw new Exception("id must be set for VarDecl")
      new OCTemplate(ArrayBuffer.empty[String])
      /*
      new OCTemplate(ArrayBuffer[String]
        ("A9", "00", "8D", "T"+id.get, "XX"))
      */
    }

    case 'StringDecl => {
      if (id.isEmpty) throw new Exception("id must be set for VarDecl")
      new OCTemplate(ArrayBuffer.empty[String])
    }

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


    case 'HALT => {
      new OCTemplate(ArrayBuffer[String]
        ("00"))
    }

    case _ => emptyTemplate
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
