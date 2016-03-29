package compy

import scala.collection.mutable.ArrayBuffer

class Analyzer(var rootNode: Node) {
  
  var errorState = false
  //val errors = ArrayBuffer.empty[
  var errorString = ""

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
    errorString += ("Semantic Error: types " + var1.varType + " and " + var2.varType
        + " are not comparable; found at line " + node.token.get.line + "\n")
  }

  private def checkDeclared(node: Node): Boolean = {
    val declared = !getVariable(node).isEmpty
    if (!declared)
      undeclaredError(node)
    return declared
    /*
    var parent = node.getParentNode('Block)
    var declared = false
    while (!declared && !parent.isEmpty) {
      if (!parent.get.tableNode.isEmpty)
        declared = parent.get.tableNode.get.contains(node.token.get.string)
      parent = parent.get.getParentNode('Block)
    }
    if (!declared)
      undeclaredError(node)
    return declared
    */
  }

  /**
   * I have not tested this thuroughly, but it probably works.
   */
  private def getVariable(node: Node): Option[SymbolEntry] = {
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

  // This is grammar specific, but I think this is the
  // best way at least for now.
  private def addSymbol(parent: Node, node: Node): Boolean = {
    if (parent.tableNode.isEmpty)
      parent.tableNode = Some(new TableNode())
    val id = node.children(1).token.get.value
    if (parent.tableNode.get.contains(id)) {
      redeclarationError(node.children(1))
      return false
    }
    parent.tableNode.get += ((id,
        new SymbolEntry(Symbol(node.children(0).token.get.string))))
    true
  }

  private def checkBooleanExpr(node: Node): Boolean = {
    val arg1 = node.children(1).children(0)
    val arg2 = node.children(3).children(0)
    val var1 = getVariable(arg1)
    val var2 = getVariable(arg2)
    var comparable = false
    if (!var1.isEmpty && !var2.isEmpty) {
      comparable = var1.get.varType == var2.get.varType
      if (!comparable)
        booleanExprError(node.children(2).children(0), var1.get, var2.get)
    }
    return comparable
  }

  private def checkAssign(node: Node): Boolean = {
    val variable = getVariable(node.children(0))
    if (variable.isEmpty)
      return false
    var valid = false
    variable.get.varType match {
      case 'int => valid = (node.children(2).children(0).symbol == 'IntExpr)
      case _ => {}
    }
    println(valid)
    return valid
  }

  def analyzeTree = {
    analyze(rootNode)
  }

  /**
   * An in-order traversal of the parse tree.
   */
  private def analyze(node: Node): Unit = {
    node.symbol match {
      case 'VarDecl => addSymbol(node.getParentNode('Block).get, node)
      case 'id => checkDeclared(node)
      case 'BooleanExpr => println(checkBooleanExpr(node))
      case _ => {}
    }
    /*
    if (node.symbol == 'VarDecl) {
      addSymbol(node.getParentNode('Block).get, node)
    } else if (node.symbol == 'id) {
      checkDeclared(node)
    }
    */
    if (!node.children.isEmpty)
      node.children.foreach((child: Node) => {
        analyze(child)
      })
  }
}
