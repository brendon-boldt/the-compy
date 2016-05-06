package compy

class Generator(val rootNode: Node) {

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
      case 'VarDecl => {
        //if (c(0).symbol != 'string) {
          executable.varDecl(Analyzer.getVariable(c(1)).get)
        //}
      }

          
      case 'PrintStatement => {
        if (Analyzer.getType(c(0)) != 'string) {
          executable.printLit(Analyzer.getVariable(c(0)).get)
        } else if (c(0).symbol == 'stringlit) {
          executable.printStringLit(c(0).token.get.value)          
        } else {
          executable.printString(Analyzer.getVariable(c(0)).get)
        }
      }

      case _ => ()
    }
  }

  def postApply(node: Node): Unit = {

    def c = node.children
    node.symbol match {

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
