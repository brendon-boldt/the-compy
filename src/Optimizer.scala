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
          node.symbol = 'digit
          return Array.empty[Node]
        } else if (c(1).symbol == 'intop) {
          node.token.get.value =
            (c(0).token.get.value.toInt + c(1).children(0).token.get.value.toInt).toString
          node.token.get.string = node.token.get.value
          node.symbol = 'digit
        }
        return c
      }
      
      case 'eq => return foldEquivalent(node)
      case 'neq => return foldEquivalent(node)

      case _ => return c
    }

  }

  private def foldEquivalent(node: Node): Array[Node] = {
    def c = node.children
    if (c(0).symbol == c(1).symbol && (
        c(0).symbol == 'digit ||
        c(0).symbol == 'stringlit ||
        c(0).symbol == 'boolval
      )){
      if (node.symbol != 'eq ^ (c(0).token.get.value == c(1).token.get.value)) 
        node.token.get.value = "true"
      else
        node.token.get.value = "false"
      node.token.get.string = node.token.get.value
      node.symbol = 'boolval
      return Array.empty[Node]
    }
    return c
  }


  private def optimize(node: Node): Array[Node] = {
    
    node.children.foldLeft(Array.empty[Node])((arr: Array[Node], n: Node) => {
      arr ++ optimize(n)
    })
    
    val results = Optimizer.applyRule(node)
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
