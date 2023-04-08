package parser.ast

import parser.ast.Statement.CompoundStm
import token.Token

trait Definition extends Statement

object Definition:

  case class StructDef(id: Token, varDefinitions: VariableDef*) extends Definition

  case class VariableDef(typeBase: Aux.TypeBase, id: Token, arraySize: Option[Aux.ArraySize] = None) extends Definition

  case class FunctionDef(typeBase: Aux.TypeBase, id: Token, stmCompound: CompoundStm, fnParams: Aux.FunctionParam*) extends Definition
