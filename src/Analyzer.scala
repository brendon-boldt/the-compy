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

  private def checkDeclared(node: Node): Boolean = {
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

  def analyzeTree = {
    analyze(rootNode)
  }

  /**
   * An in-order traversal of the parse tree.
   */
  private def analyze(node: Node): Unit = {
    if (node.symbol == 'VarDecl) {
      addSymbol(node.getParentNode('Block).get, node)
    } else if (node.symbol == 'id) {
      checkDeclared(node)
    }
    if (!node.children.isEmpty)
      node.children.foreach((child: Node) => {
        analyze(child)
      })
  }
}
