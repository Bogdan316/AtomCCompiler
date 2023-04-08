package parser.ast

import token.TokenCode.ID
import token.{Token, TokenWithValue}

trait Aux extends AstNode

object Aux:

  case class TypeBase(baseType: Token, structId: Option[Token] = None) extends Aux

  case class ArraySize(size: Token = TokenWithValue(ID, -1, 0)) extends Aux

  case class FunctionParam(typeBase: TypeBase, id: Token, arraySize: Option[ArraySize] = None) extends Aux
