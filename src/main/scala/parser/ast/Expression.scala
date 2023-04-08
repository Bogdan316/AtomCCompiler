package parser.ast

import token.{Token, TokenWithValue}

trait Expression extends AstNode

object Expression:
  
  case class BinaryExpr(left: Expression, operator: Token, right: Expression) extends Expression
  
  case class UnaryExpr(operator: Token, right: Expression) extends Expression
  
  case class FunctionCallExpr(funName: Token, expressions: Expression*) extends Expression
  
  case class LiteralExpr[T](literal: TokenWithValue[T]) extends Expression
  
  case class VariableExpr[T](variable: TokenWithValue[T]) extends  Expression
  
  case class CastExpr(typeBase: Aux.TypeBase, arraySize: Option[Aux.ArraySize] = None, castedExpr: Expression) extends Expression
