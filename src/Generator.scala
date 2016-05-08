package compy

class Generator(val rootNode: Node) {

  var flagVerbose = false
  def vPrint(s: String): Unit = if (flagVerbose) println("GENERATOR: " + s)

  val executable = new Executable

  def generateExecutable(): Executable = {
    generate(rootNode)
    executable.insert(OCTemplate('HALT))
    executable.addressStatic
    if (executable.outOfMemory) {
      throw new Exception("Executable has run out of memory.")
    }
    executable.backpatch
    return executable
  }

  // Everything might have to be moved to post assign.
  // I think this should be a leaf-first traversal
  def preApply(node: Node): Unit = {
    def c = node.children
    node.symbol match {
      case 'eq => {
        if ((c(0).symbol == 'intop || c(0).symbol == 'eq || c(0).symbol == 'neq)
          && (c(1).symbol == 'intop || c(1).symbol == 'eq || c(1).symbol == 'neq)) {
            println("Setting accToM")
            executable.memTable += c(0) -> new StaticEntry((executable.memCounter+48).toChar)
            executable.memCounter += 1
            // I am going to need a M memory table if I want this all to work
          }
      }

      case 'neq => {

      }
      
      case _ => ()
    }
  }

  /**
   * This generation will fail if the optimizer is not run;
   * this is so because there is no OCT for two constants
   * comppared to each other.
   */
  def applyCompareLit(equ: Boolean, c: Array[Node]): Unit = {
    for ( a <- Range(0,2) ) {
      val b = (a + 1) % 2
      println(a + "" + b)
      if (c(a).symbol == 'id
        && (c(b).symbol == 'eq || c(b).symbol == 'neq || c(b).symbol == 'intop)) {
        executable.compareVarAcc(equ,
          Analyzer.getVariable(c(a)).get)
        return ()
      } else if ( (c(a).symbol == 'digit || c(a).symbol == 'boolval)
        && (c(b).symbol == 'eq || c(b).symbol == 'neq || c(b).symbol == 'intop)) {
        //println(
        executable.compareLitAcc(equ,
          c(a).token.get.value)
        return ()
      } else if (c(a).symbol == 'id && (c(b).symbol == 'digit || c(b).symbol == 'boolval)) {
        executable.compareLitVar(equ,
          Analyzer.getVariable(c(a)).get, c(b).token.get.value)
        return ()
      } else if (c(a).symbol == 'id && c(b).symbol == 'id) {
        executable.compareVarVar(equ,
          Analyzer.getVariable(c(a)).get,
          Analyzer.getVariable(c(b)).get)
        return ()
      } else if ((c(a).symbol == 'intop || c(a).symbol == 'eq || c(a).symbol == 'neq)
        && (c(b).symbol == 'intop || c(b).symbol == 'eq || c(b).symbol == 'neq)) {
        executable.compareAccAcc(equ, c(0))
        //executable.compareAccAcc(equ, executable.accToMMap.get(c(0)).get)
        return ()
      }
      //} else if (c(1).symbol == 'id) {
      //  executable.compareLitVar(equ,
      //    Analyzer.getVariable(c(1)).get, c(0).token.get.value)
      //} else
    }
      throw new Exception("No code generation for comparing two constants")
  }

  def applyCompareString(eq: Boolean, c: Array[Node]): Unit = {
  }

  def postApply(node: Node): Unit = {

    def c = node.children
    node.symbol match {
      case 'VarDecl => {
        //if (c(0).symbol != 'string) {
          executable.varDecl(Analyzer.getVariable(c(1)).get)
        //}
      }

      case 'eq => {
        if (c(0).symbol == 'stringlit || c(1).symbol == 'stringlit)
          applyCompareString(true, c)
        else
          applyCompareLit(true, c)
        if (executable.memTable.contains(node)) {
          println("Storing Acc to M")
          executable.storeAccToM(node)
        }
      }
      case 'neq => {
        ???
        if (c(0).symbol == 'stringlit || c(1).symbol == 'stringlit)
          applyCompareString(false, c)
        else
          applyCompareLit(false, c)
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
        //println("assign with " + varType)
        if (c(1).symbol == 'digit
            || c(1).symbol == 'boolval) {
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
    // Could be val, but that might cause a problem
    preApply(node)

    if (!node.children.isEmpty)
      for ( child <- node.children )
        generate(child)

    postApply(node)
  }
}
