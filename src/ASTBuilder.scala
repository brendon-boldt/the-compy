package compy

object ASTBuilder {

  def applyRule(node: Node): Array[Node] = {
    def c = node.children
    node.symbol match {
      case 'Program => {
        val block = c(0)
        return Array(block)
      }

      case 'Block => {
        node.setChildren(applyRule(c(1)))
        return c
      }
      
      case 'StatementList => {
        if (c.size == 1)
          return Array.empty[Node]
        return applyRule(c(1)).+:(c(0).children(0))
      }

      case 'AssignStatement => {
        return applyRule(c(2)).+:(c(0))
      }

      case 'WhileStatement => {
        return applyRule(c(1)).:+(c(2))
      }

      case 'IfStatement => {
        return applyRule(c(1)).:+(c(2))
      }

      case 'PrintStatement => {
        return applyRule(c(2))
      }

      case 'Expr => {
        if (c(0).symbol == 'id)
          return Array(c(0))
        return applyRule(c(0))
      }

      case 'IntExpr => {
        if (c.size == 1)
          return c
        val intop = c(1)
        intop.setChildren(applyRule(c(2)).+:(c(0)))
        return Array(intop)
      }
  
      case 'BooleanExpr => {
        if (c.size == 1)
          return c
        val boolop = c(2).children(0)
        boolop.setChildren(
          applyRule(c(1)) ++ applyRule(c(3))
        )
        return Array(boolop)
      }
    
      case _ => return c
    }
  }
}

class ASTBuilder(val cst: Node) {

  private var rootNode: Option[Node] = None

  def getAST(): Option[Node] = {
    return rootNode
  }

  def buildAST(): Node = {
    rootNode = Some(analyze(cst)(0))
    return rootNode.get
  }

  private def analyze(node: Node): Array[Node] = {
    val results = ASTBuilder.applyRule(node)
    results.foldLeft(Array.empty[Node])((arr: Array[Node], n: Node) => {
      arr ++ analyze(n)
    })
    node.setChildren(results)
    return results
  }
}
