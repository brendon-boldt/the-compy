package compy

// A CST node is constructed from a symbol, an array of children, and token
class Node(var symbol: Symbol, var children: Array[Node], var value: Option[Token] = None) {
  
  // Nodes are usually constructed before the children are known.
  // Hence, we must have a way to set the children post-construction.
  def setChildren(children: Array[Node]) {
    this.children = children
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
  def getTreeString(level: Int): String = {
    var string = "\t"*level + this.toString
    if (!children.isEmpty)
      string += "\n"+children.map(_.getTreeString(level+1)+"\n").reduce(_+_)
    string.stripLineEnd
  }

  // The README for a link to a visualizer for the bracketed tree
  def getTreeBrackets(): String = {
    var string = this.symbol.toString
    if (this.value.nonEmpty)
      string += " " + this.value.get.string
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
