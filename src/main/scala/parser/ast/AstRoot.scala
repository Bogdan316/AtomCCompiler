package parser.ast

trait AstRoot extends AstNode

object AstRoot:

  case class UnitRule(definitions: Definition*) extends AstNode
  