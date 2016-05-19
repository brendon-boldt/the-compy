package compy

import scala.collection.mutable.ArrayBuffer

object Analyzer {

  var flagVerbose = false

  /**
   * Print only in verbose mode
   */
  def vPrint(s: String) = {
    if (flagVerbose)
      println("ANALYZER: " + s)
  }

  /**
   * Gets the variable specified by the node
   * I have not tested this thuroughly, but it probably works.
   *
   * This might ought to be in a class of its own.
   */
  def getVariable(node: Node): Option[SymbolEntry] = {
    if (node.symbol != 'id) {
      throw new Exception("Cannot get variable with a non-ID node")
    }
    //vPrint("Attempting to get variable " + node.token.get.string)
    var parent = node.getParentNode('Block)
    var variable: Option[SymbolEntry] = None
    while (variable.isEmpty && !parent.isEmpty) {
      if (!parent.get.tableNode.isEmpty) {
        variable = parent.get.tableNode.get.get(node.token.get.string)
      }
      parent = parent.get.getParentNode('Block)
    }
    return variable
  }

  /**
   * Gets the type of any node. Most useful for ID's, intops,
   * and other expressions
   */
  def getType(node: Node): Symbol = node.symbol match {
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

  var flagVerbose = false

  def vPrint(s: String) = {
    if (flagVerbose)
      println("ANALYZER: " + s)
  }

  var errorState = false
  var errorString = ""
  var warningState = false
  var warningString = ""

  private def unusedWarning(se: SymbolEntry) {
    warningState = true
    warningString += ("Semantic Warning: variable " + se.token.value
        + " was not used; declared at line " + se.token.line + "\n")
  }

  /**
   * At the end of a block, this checks whether each variable has been used
   */
  private def checkUnused(node: Node) = {
    if (node.symbol != 'Block) {
      throw new Exception("Cannot check unsed on a non-block node")
    }
    if (!node.tableNode.isEmpty) {
      // Sort this by line number
      node.tableNode.get.foreach( (tu:(String, SymbolEntry)) => {
        vPrint("Checking if " + tu._1 + " has been used: " + tu._2.isUsed)
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
    vPrint("Checking if " + se + " has been initialized: " + se.initialized)
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
        + node.children(1).token.get.line + "\n")
  }

  /**
   * Checks if the variable specified by the (ID) node has been declared
   */
  private def checkDeclared(node: Node): Boolean = {
    val varOpt = Analyzer.getVariable(node)
    val declared = !varOpt.isEmpty
    if (!declared)
      undeclaredError(node)
    else {
      // Increment the number of uses; this is needed otherwise
      // `int x` would count as a use
      varOpt.get.uses += 1
      node.symbolEntry = varOpt
    }
    vPrint("Checking if " + node.token.get.string + " has been declared: " + declared)
    return declared
  }

  /**
   * Adds a variable to the symbol table of the current scope (block)
   */
  private def addSymbol(parent: Node, node: Node): Boolean = {
    // Make a new symbol table if there is not one already on the block
    if (parent.tableNode.isEmpty)
      parent.tableNode = Some(new TableNode())
    val id = node.children(1).token.get.value
    // Check if the variable already exists in this scope
    if (parent.tableNode.get.contains(id)) {
      redeclarationError(node.children(1))
      return false
    }
    vPrint("Adding " + id + " to the symbol table")
    // Actually add it to the table (hash map)
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

  /**
   * Checks the validity of a boolean expression (comparison)
   */
  private def checkBooleanExpr(node: Node): Boolean = {
    vPrint("Checking boolean expression " + node)
    val arg1 = node.children(0)
    val arg2 = node.children(1)
    var type1 = checkExprType(arg1)
    var type2 = checkExprType(arg2)

    if (type1 == 'unit || type2 == 'unit)
      return false
    return Analyzer.getType(arg1) == Analyzer.getType(arg2)  
  }

  /**
   * Checks the validity of an assignment statement
   */
  private def checkAssign(node: Node): Boolean = {
    val variable = Analyzer.getVariable(node.children(0))
    if (variable.isEmpty)
      return false
    var valid = false
    val assignType = checkExprType(node.children(1))
    variable.get.uses -= 1
    variable.get.varType match {
      // The reason why 'invalidType is considered not an error here is because
      // the type error would already have been registered as an intop error and
      // we do not want to report this twice.
      case 'int => valid = (assignType == 'int
        || assignType == 'unit
        || assignType == 'invalidType)
      case 'string => valid = (assignType == 'string)
      case 'boolean => valid = (assignType == 'boolean)
      case _ => {}
    }
    if (!valid)
      assignmentError(node, variable.get.varType)
    variable.get.initialized = true
    vPrint("Checking assignment " + node + ": " + valid)
    return valid
  }

  /**
   * Checks the validity of an intop
   */
  private def checkIntop(node: Node): Boolean = {
    // intop's can only involve int's
    if (checkExprType(node.children(1)) != 'int) {
      intopError(node: Node)
      vPrint("Checking intop " + node + ": false")
      return false
    }
    vPrint("Checking intop " + node + ": true")
    return true
  }

  /**
   * Checks the validity of a print statement; this only includes
   * checking that the expression uses initialized variables
   */
  private def checkPrint(node: Node): Boolean = {
    checkExprType(node.children(0))
    return true
  }

  /**
   * The initial method to be called to initiate the analysis
   */
  def analyzeTree = {
    analyze(rootNode)
    if (errorState)
      println(errorString.stripLineEnd)
    if (warningState)
      println(warningString.stripLineEnd)
  }

  /**
   * An in-order traversal of the parse tree.
   */
  private def analyze(node: Node): Unit = {
    vPrint("Analyzing node " + node.symbol)
    // The following switches determine what is to be done with each CST node
    node.symbol match {
      case 'VarDecl => addSymbol(node.getParentNode('Block).get, node)
      case 'id => checkDeclared(node)
      case 'eq => checkBooleanExpr(node)
      case 'neq => checkBooleanExpr(node)
      case 'PrintStatement => checkPrint(node)
      case 'intop => checkIntop(node)
      case _ => {}
    }

    // Yay recursion!
    if (!node.children.isEmpty)
      for ( child <- node.children )
        analyze(child)

    node.symbol match {
      case 'Block => checkUnused(node)
      case 'AssignStatement => checkAssign(node)
      case _ => {}
    }
  }
}
