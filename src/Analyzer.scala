package compy

class Analyzer(var rootNode: Node) {
  
  var errorState = false

  // This is grammar specific, but I think this is the
  // best way at least for now.
  def addSymbol(parent: Node, node: Node): Boolean = {
    if (parent.tableNode.isEmpty)
      parent.tableNode = Some(new TableNode())
    parent.tableNode.get += ((node.children(1).value.get.value,
        new SymbolEntry(Symbol(node.children(0).value.get.string))))
    true
  }

  def analyzeTree = {
    analyze(rootNode)
  }

  /**
   * An in-order traversal of the parse tree.
   */
  private def analyze(node: Node): Unit = {
    if (node.children.isEmpty)
      return ()
    if (node.symbol == 'VarDecl) {
      errorState = (errorState
          || !addSymbol(node.getParentNode('Block).get, node))
    }
    node.children.foreach((child: Node) => {
      analyze(child)
    })
  }
}
