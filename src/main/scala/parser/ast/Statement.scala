package parser.ast

import token.Token

trait Statement extends AstNode

object Statement:

  case class CompoundStm(statements: Statement*) extends Statement

  case class IfStm(condition: Expression, thenBranch: Statement, elseBranch: Option[Statement] = None) extends Statement

  case class WhileStm(condition: Expression, body: Statement) extends Statement

  case class ReturnStm(expr: Option[Expression] = None) extends Statement

  case class ExpressionStm(expr: Option[Expression] = None) extends Statement
