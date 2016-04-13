package compy

import scala.collection.mutable.ArrayBuffer

object Analyzer {

  /**
   * I have not tested this thuroughly, but it probably works.
   */
  private def getVariable(node: Node): Option[SymbolEntry] = {
    if (node.symbol != 'id) {
      throw new Exception("Cannot get variable with a non-ID node")
    }
    var parent = node.getParentNode('Block)
    //var declared = false
    var variable: Option[SymbolEntry] = None
    while (variable.isEmpty && !parent.isEmpty) {
      if (!parent.get.tableNode.isEmpty) {
        variable = parent.get.tableNode.get.get(node.token.get.string)
      }
      parent = parent.get.getParentNode('Block)
    }
    return variable
  }

  private def getType(node: Node): Symbol = node.symbol match {
    case 'id => {
      val se = getVariable(node)
      if (se.isEmpty)
        return 'unit
      else {
        return se.get.varType
      }
    }
    case 'intop => {
      val varType = getType(node.children(1))
      if (varType == 'int)
        return 'int
      else
        return 'invalidType
    }
    case 'digit => return 'int
    case 'eq  => return 'boolean
    case 'neq => return 'boolean
    case 'stringlit => return 'string
    case 'boolval => return 'boolean
    case _ => return 'unit
  }
}

class Analyzer(var rootNode: Node) {
  
  var errorState = false
  var errorString = ""
  var warningState = false
  var warningString = ""

  private def unusedWarning(se: SymbolEntry) {
    warningState = true
    warningString += ("Semantic Warning: variable " + se.token.value
        + " was not used; declared at line " + se.token.line + "\n")
  }

  private def checkUnused(node: Node) = {
    if (node.symbol != 'Block) {
      throw new Exception("Cannot check unsed on a non-block node")
    }
    if (!node.tableNode.isEmpty) {
      // Sort this by line number
      node.tableNode.get.foreach( (tu:(String, SymbolEntry)) => {
        println(tu._2.uses)
        if (!tu._2.isUsed)
          unusedWarning(tu._2)
      })
    }
  }

  private def uninitializedWarning(node: Node) = {
    warningState = true
    warningString += ("Semantic Warning: variable " + node.token.get.value
        + " was not initialized; used at line " + node.token.get.line + "\n")
  }

  private def checkInitialized(se: SymbolEntry): Boolean = {
    return se.initialized
  }

  private def redeclarationError(node: Node) = {
    errorState = true
    errorString += ("Semantic Error: redeclaration of " + node.token.get.value
        + " at line " + node.token.get.line + "\n")
  }

  private def undeclaredError(node: Node) = {
    errorState = true
    errorString += ("Semantic Error: variable " + node.token.get.value
        + " was not declared; found at line " + node.token.get.line + "\n")
  }

  private def booleanExprError(node: Node, var1: SymbolEntry, var2: SymbolEntry) = {
    errorState = true
    errorString += ("Semantic Error: types " + var1.varString + " and " + var2.varString
        + " are not comparable; found at line " + node.token.get.line + "\n")
  }

  private def assignmentError(node: Node, varType: Symbol) = {
    errorState = true
    errorString += ("Semantic Error: " + Analyzer.getType(node.children(1)).name
        + " cannot be assigned to " + node.children(0).token.get.value + " of type "
        + varType.name + "; found on line " + node.children(0).token.get.line + "\n")
  }

  private def intopError(node: Node) {
    errorState = true
    errorString += ("Semantic Error: cannot add type int to type "
        + Analyzer.getType(node.children(1)).name + "; found on line "
        + node.children(1).token.get.line)
  }


  private def checkDeclared(node: Node): Boolean = {
    val varOpt = Analyzer.getVariable(node)
    val declared = !varOpt.isEmpty
    if (!declared)
      undeclaredError(node)
    else {
      varOpt.get.uses += 1
    }
    return declared
  }

  private def addSymbol(parent: Node, node: Node): Boolean = {
    if (parent.tableNode.isEmpty)
      parent.tableNode = Some(new TableNode())
    val id = node.children(1).token.get.value
    if (parent.tableNode.get.contains(id)) {
      redeclarationError(node.children(1))
      return false
    }
    parent.tableNode.get += ((id,
        new SymbolEntry(node.children(1).token.get,
          Symbol(node.children(0).token.get.string))))
    true
  }

  /**
   * Similar to getType, except it checks that the variable is initialized
   */
  private def checkExprType(node: Node): Symbol = {
    if (node.symbol == 'id) {
      val vari = Analyzer.getVariable(node) 
      if (!vari.isEmpty){
        if (!checkInitialized(vari.get))
          uninitializedWarning(node)
        return vari.get.varType
      }
      return 'unit
    } else 
      return Analyzer.getType(node)
  }

  private def checkBooleanExpr(node: Node): Boolean = {
    val arg1 = node.children(0)
    val arg2 = node.children(1)
    var type1 = checkExprType(arg1)
    var type2 = checkExprType(arg2)

    if (type1 == 'unit || type2 == 'unit)
      return false
    return Analyzer.getType(arg1) == Analyzer.getType(arg2)  
  }

  private def checkAssign(node: Node): Boolean = {
    val variable = Analyzer.getVariable(node.children(0))
    if (variable.isEmpty)
      return false
    var valid = false
    //val assignType = Analyzer.getType(node.children(1))
    val assignType = checkExprType(node.children(1))
    variable.get.varType match {
      // The reason why 'invalidType is considered not an error here is because
      // the type error would already have been registered as an intop error and
      // we do not want to report this twice.
      case 'int => valid = (assignType == 'int || assignType == 'invalidType)
      case 'string => valid = (assignType == 'string)
      case 'boolean => valid = (assignType == 'boolean)
      case _ => {}
    }
    if (!valid)
      assignmentError(node, variable.get.varType)
    variable.get.initialized = true
    return valid
  }

  private def checkIntop(node: Node): Boolean = {
    if (checkExprType(node.children(1)) != 'int) {
    //if (Analyzer.getType(node.children(1)) != 'int) {
      intopError(node: Node)
      return false
    }
    return true
  }


  def analyzeTree = {
    analyze(rootNode)
  }

  /**
   * An in-order traversal of the parse tree.
   */
  private def analyze(node: Node): Unit = {
    //println(node.symbol + " => " + getType(node))
    node.symbol match {
      case 'VarDecl => addSymbol(node.getParentNode('Block).get, node)
      case 'id => checkDeclared(node)
      case 'eq => println("eq: " + checkBooleanExpr(node))
      case 'neq => println("neq: " + checkBooleanExpr(node))
      case 'AssignStatement => println("Assign: " + checkAssign(node))
      case 'intop => println("intop: " + checkIntop(node))
      case _ => {}
    }

    if (!node.children.isEmpty)
      node.children.foreach((child: Node) => {
        analyze(child)
      })

    node.symbol match {
      case 'Block => checkUnused(node)
      case _ => {}
    }
  }
}
