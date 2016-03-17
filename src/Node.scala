package compy

// A CST node is constructed from a symbol, an array of children, and token
class Node(var symbol: Symbol,
    var children: Array[Node] = Array.empty[Node],
    var token: Option[Token] = None,
    var parent: Option[Node] = None) {
  
  var tableNode: Option[TableNode] = None

  // Nodes are usually constructed before the children are known.
  // Hence, we must have a way to set the children post-construction.
  def setChildren(children: Array[Node]) {
    this.children = children
    //this.children.foreach( _.parent = Some(this) )
  }

  /**
   * Never returns the node this is initially called on
   */
  def getParentNode(symbol: Symbol): Option[Node] = {
    //if (this.symbol == symbol)
      //return Some(this)
    /*else*/ if (parent.isEmpty)
      return None
    else if (parent.get.symbol == symbol)
      return parent
    else
      return parent.get.getParentNode(symbol)
  }

  // Leaf seems to be an appropriate term for a person without children
  def isLeaf(): Boolean = {
    children.isEmpty
  }

  override def toString(): String = {
    //symbol.toString
    getTreeBrackets
  }

  // This generates a tree using tabs to denote levels; it's quite hideous
  def getTreeString(level: Int = 0): String = {
    var parentString = "None"
    if (!this.parent.isEmpty)
      parentString = this.parent.get.symbol.toString
    var string = "\t"*level + this.symbol.toString// + " --> " + parentString
    if (!this.tableNode.isEmpty) {
      string += "\n" + this.tableNode.get.toString + "\n"
    }
    if (!children.isEmpty)
      string += "\n"+children.map(_.getTreeString(level+1)+"\n").reduce(_+_)
    string.stripLineEnd
  }

  def getSTString(level: Int = 0): String = {
    var string = ""
    if (this.symbol != 'Block) {
      if (!children.isEmpty)
        string += children.map(_.getSTString(level)).reduce(_+_)
      string.stripLineEnd
    } else {
      string = ("\t"*level) + this.symbol.toString + "\n"
      if (!this.tableNode.isEmpty) {
        //string += ("\t"*level)+this.tableNode.get.toString + "\n"
        string += this.tableNode.get.toString
      }
      if (!children.isEmpty)
        string += children.map(_.getSTString(level+1)).reduce(_+_)
      string.stripLineEnd
    }
  }

  // The README for a link to a visualizer for the bracketed tree
  def getTreeBrackets(): String = {
    var string = this.symbol.toString
    if (this.token.nonEmpty)
      string += " " + this.token.get.string
    if (!children.isEmpty)
      string += children.map("["+_.getTreeBrackets+"]").reduce(_+_)
    string 
  }

  // Gets the length of the node by counting its non-epsilon leaves
  def getLength(): Int = {
    if (symbol == 'epsilon)
      return 0
    else if (isLeaf)
      return 1
    else
      children.map(_.getLength).reduce(_+_)
  }
}
