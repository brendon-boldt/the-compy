package compy

object Optimizer {

  def applyRule(node: Node): Array[Node] = {
    def c = node.children
    //vPrint("Applying AST rule for " + node.symbol)
    node.symbol match {

      case 'intop => {
        if (c(1).symbol == 'digit) {
          node.token.get.value =
            (c(0).token.get.value.toInt + c(1).token.get.value.toInt).toString
          node.token.get.string = node.token.get.value
          return Array.empty[Node]
        }
        return c
      }

      case _ => return c
    }
  }


  private def optimize(node: Node): Array[Node] = {
    val results = Optimizer.applyRule(node)
    // Analyze all of the children of the current node
    
    results.foldLeft(Array.empty[Node])((arr: Array[Node], n: Node) => {
      arr ++ optimize(n)
    })
    
    node.setChildren(results)
    return results
  }
 
}


class Optimizer(val rootNode: Node) {

  def optimizeTree(): Node = {
    Optimizer.optimize(rootNode)
    return rootNode
  }


}
