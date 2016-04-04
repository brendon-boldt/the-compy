package compy

object ASTBuilder {
  /*
  val rules = collection.immutable.HashMap[Symbol, Node => Option[Node]](
    
    'Program -> (node: Node) => {
      val block = node.children(0)
      block.setChildren(block.children.slice(1,2))
      return block
    }
  )
  */

  def applyRule(node: Node): Array[Node] = {
    node.symbol match {
      case 'Program => {
        val block = node.children(0)
        return Array(block)
      }

      case 'Block => {
        node.setChildren(ASTBuilder.applyRule(node.children(1)))
        return node.children
      }
      
      case 'StatementList => {
        if (node.children.size == 1)
          return Array.empty[Node]
        return ASTBuilder.applyRule(node.children(1)).+:(node.children(0).children(0))
      }

      case 'AssignStatement => {
        return ASTBuilder.applyRule(node.children(2)).+:(node.children(0))
      }

      case 'WhileStatement => {
        return ASTBuilder.applyRule(node.children(1)).:+(node.children(2))
      }

      case 'IfStatement => {
        return ASTBuilder.applyRule(node.children(1)).:+(node.children(2))
      }

      case 'Expr => {
        if (node.children(0).symbol == 'id)
          return Array(node.children(0))
        return ASTBuilder.applyRule(node.children(0))
      }

      case 'IntExpr => {
        if (node.children.size == 1)
          return node.children
        val intop = node.children(1)
        intop.setChildren(ASTBuilder.applyRule(node.children(2)).+:(node.children(0)))
        return Array(intop)
      }
  
      case 'BooleanExpr => {
        if (node.children.size == 1)
          return node.children
        val boolop = node.children(2).children(0)
        boolop.setChildren(
          ASTBuilder.applyRule(node.children(1)) ++ ASTBuilder.applyRule(node.children(3))
        )
        return Array(boolop)
      }
    
      case _ => return node.children
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
