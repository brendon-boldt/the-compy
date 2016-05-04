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

  private def generate(node: Node): Unit = {

    // Could be val, but that might cause a problem
    def c = node.children
    node.symbol match {
      case 'VarDecl => {
        //if (c(0).symbol != 'string) {
          executable.varDecl(Analyzer.getVariable(c(1)).get)
        //}
      }

      case 'AssignStatement => {
        //if (Analyzer.getType(c(0)) != 'string) {
          executable.assignStatement(Analyzer.getVariable(c(0)).get,
            c(1).token.get.value)
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

    if (!node.children.isEmpty)
      for ( child <- node.children )
        generate(child)
  }
}
