package compy


object Node {
  
  def fromSymbols(symbol: Symbol, children: Array[Symbol]): Node = {
    new Node(symbol, children.map(new Node(_,Array.empty[Node])))
  }
}

// Need this include a token?
class Node(var symbol: Symbol, var children: Array[Node], var value: Option[Token] = None) {
  
  def setChildren(children: Array[Node]) {
    this.children = children
  }

  def isLeaf(): Boolean = {
    children.isEmpty
  }

  override def toString(): String = {
    //symbol.toString
    getTreeBrackets
  }

  def getTreeString(level: Int): String = {
    var string = "\t"*level + this.toString
    if (!children.isEmpty)
      string += "\n"+children.map(_.getTreeString(level+1)+"\n").reduce(_+_)
    string.stripLineEnd
  }

  def getTreeBrackets(): String = {
    var string = this.symbol.toString
    if (this.value.nonEmpty)
      string += " " + this.value.get.string
    if (!children.isEmpty)
      string += children.map("["+_.getTreeBrackets+"]").reduce(_+_)
    string 
  }

  // Excludes epsilon
  def getLength(): Int = {
    if (symbol == 'epsilon)
      return 0
    else if (isLeaf)
      return 1
    else
      children.map(_.getLength).reduce(_+_)
  }
}