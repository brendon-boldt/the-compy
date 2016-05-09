package compy

import scala.collection.mutable.Set

class Generator(val rootNode: Node) {

  var flagVerbose = false
  def vPrint(s: String): Unit = if (flagVerbose) println("GENERATOR: " + s)

  val executable = new Executable

  val noStoreSet = Set.empty[Node]

  def generateExecutable(): Executable = {
    executable.flagVerbose = flagVerbose
    generate(rootNode)
    executable.insert(OCTemplate('HALT))
    executable.addressStatic
    if (executable.outOfMemory) {
      throw new Exception("Executable has run out of memory.")
    }
    executable.backpatch
    return executable
  }

  def preApplyBoolOp(c: Array[Node]): Unit = {
    if ((c(0).symbol == 'intop || c(0).symbol == 'eq || c(0).symbol == 'neq)
      && (c(1).symbol == 'intop || c(1).symbol == 'eq || c(1).symbol == 'neq)) {
        executable.memTable += c(0) -> new StaticEntry((executable.memCounter+48).toChar)
        executable.memCounter += 1
      }
  }

  def preApply(node: Node): Unit = {
    vPrint("Pre-applying " + node.symbol)
    def c = node.children
    node.symbol match {
      case 'eq => {
        preApplyBoolOp(c)
      }

      case 'neq => {
        preApplyBoolOp(c)
      }
      
      case 'IfStatement => {
        if (c(0).symbol == 'boolval) {
          if (c(0).token.get.value == "false") {
            node.setChildren(Array.empty[Node])
          }
        } else {
          noStoreSet += c(0)
          generate(c(0))
          val equ = c(0).symbol == 'eq
          node.setChildren(c.slice(1,2))
          executable.ifStatement(node, equ)
        }
      }

      case 'WhileStatement => {
        if (c(0).symbol == 'boolval && c(0).token.get.value == "false") {
          node.setChildren(Array.empty[Node])
        } else {
          noStoreSet += c(0)
          executable.pushIP
          generate(c(0))
          val equ = c(0).symbol == 'eq
          node.setChildren(c.slice(1,2))
          executable.whileStatement(node, equ)
        }
      }

      case _ => ()
    }
  }

  def isLit(s: Symbol) = (s == 'digit || s == 'boolval || s == 'stringlit)
  def isExpr(s: Symbol) = (s == 'intop || s == 'eq || s == 'neq)

  /**
   * This generation will fail if the optimizer is not run;
   * this is so because there is no OCT for two constants
   * comppared to each other.
   */
  def applyCompareLit(equ: Boolean, c: Array[Node]): Unit = {
    for ( a <- Range(0,2) ) {
      val b = (a + 1) % 2
      if (c(a).symbol == 'id
        //&& (c(b).symbol == 'eq || c(b).symbol == 'neq || c(b).symbol == 'intop)) {
        && isExpr(c(b).symbol)) {
        executable.compareVarAcc(equ,
          Analyzer.getVariable(c(a)).get)
        return ()
      } else if ( isLit(c(a).symbol) && isExpr(c(b).symbol)) {
        executable.compareLitAcc(equ,
          c(a).token.get.value)
        return ()
      } else if (c(a).symbol == 'id && isLit(c(b).symbol)) {
        executable.compareLitVar(equ,
          Analyzer.getVariable(c(a)).get, c(b).token.get.value)
        return ()
      } else if (c(a).symbol == 'id && c(b).symbol == 'id) {
        executable.compareVarVar(equ,
          Analyzer.getVariable(c(a)).get,
          Analyzer.getVariable(c(b)).get)
        return ()
      } else if (isExpr(c(a).symbol) && isExpr(c(b).symbol)) {
        executable.compareAccAcc(equ, c(0))
        return ()
      }
    }
    // All constant-constant comparisons should be optimized out at this point
    throw new Exception("No code generation for comparing two constants")
  }

  def postApply(node: Node): Unit = {
    vPrint("Post-applying " + node.symbol)
    def c = node.children
    node.symbol match {
      case 'VarDecl => {
        executable.varDecl(Analyzer.getVariable(c(1)).get)
      }

      case 'IfStatement => {
        executable.storeJump(c(0), executable.calculateJump)
      }

      case 'WhileStatement => {
        executable.postWhileStatement(node)
        val blockStart = executable.popIP
        val whileStart = executable.popIP
        executable.storeJump(c(0), executable.getIP - blockStart)
        executable.storeJump(node, 0x100 - (executable.getIP - whileStart))
        vPrint("While jump: " + (executable.getIP - blockStart + 1))
        vPrint("Wraparound jump: " + (0x100 - (executable.getIP - whileStart)))
      }

      case 'eq => {
        executable.storeZF = !noStoreSet.contains(node)

        if (c(0).symbol == 'stringlit)
          executable.createString(c(0).token.get.value)
        else if (c(1).symbol == 'stringlit)
          executable.createString(c(1).token.get.value)

        applyCompareLit(true, c)
        if (executable.memTable.contains(node)) {
          executable.storeAccToM(node)
        }
      }

      case 'neq => {
        executable.storeZF = !noStoreSet.contains(node)

        if (c(0).symbol == 'stringlit)
          executable.createString(c(0).token.get.value)
        else if (c(1).symbol == 'stringlit)
          executable.createString(c(1).token.get.value)

        applyCompareLit(false, c)
        if (executable.memTable.contains(node)) {
          executable.storeAccToM(node)
        }
      }
          
      case 'PrintStatement => {
        //if (Analyzer.getType(c(0)) != 'string) {
        val sym = c(0).symbol
        if (sym == 'digit || sym == 'boolval) {
          executable.printLit(Analyzer.getVariable(c(0)).get)
        } else if (sym == 'stringlit) {
          executable.printStringLit(c(0).token.get.value)          
        } else if (sym == 'id) {
          val variable = Analyzer.getVariable(c(0)).get
          if (variable.varType == 'string)
            executable.printString(variable)
          else
            executable.printLit(variable)
        } else {
          executable.printAcc
        }
      }

      // All constant + constant intop's have been optimized out at this point
      case 'intop => {
        executable.intop(c(0).token.get.value, Analyzer.getVariable(c(1)).get)
      }

      case 'AssignStatement => {
        val varType = Analyzer.getType(c(1))
        if (c(1).symbol == 'digit
            || c(1).symbol == 'boolval
            || c(1).symbol == 'stringlit) {
          executable.litAssign(Analyzer.getVariable(c(0)).get,
            c(1).token.get.value)
        } else if (c(1).symbol == 'intop
            || c(1).symbol == 'eq
            || c(1).symbol == 'neq) {
          executable.accAssign(Analyzer.getVariable(c(0)).get)
        }
      }

      case _ => ()
    }
  }
  
  private def generate(node: Node): Unit = {
    preApply(node)
    if (!node.children.isEmpty)
      for ( child <- node.children )
        generate(child)
    postApply(node)
  }
}
