package scope.symbol

import parser.ast.AstNode
import parser.ast.DefinitionUtils.{ArraySizeNode, TypeBaseNode}
import scope.domain.DomainManager
import scope.symbol.CompilerSymbol.StructSymbol
import scope.symbol.BaseType
import token.{Token, TokenWithValue}
import token.TokenCode.{STRUCT, TYPE_CHAR, TYPE_DOUBLE, TYPE_INT, VOID}

case class SymbolType
(
  baseType: BaseType,
  arraySize: Option[Int] = None,
  structId: Option[String] = None,
  structSymbol: Option[CompilerSymbol] = None
):
  def size: Int =
    arraySize match
      case None => structSymbol.map(s => s.size).getOrElse(baseType.size)
      case Some(0) => BaseType.TB_VOID_PTR.size
      case Some(arrSize) => structSymbol.map(s => s.size * arrSize).getOrElse(baseType.size * arrSize)

case object SymbolType:
  /**
   * Create a SymbolType object from a TypeBaseNode and a ArraySizeNode
   * @param typeBase ast node with type information
   * @param arraySize optional ast node with with an array size
   * @param structSymbol optional symbol with struct type
   * @return SymbolType
   */
  def apply(typeBase: TypeBaseNode, arraySize: Option[ArraySizeNode], structSymbol: Option[CompilerSymbol]): SymbolType =
    val baseType =
      typeBase match
        case TypeBaseNode(Token(STRUCT, _), _) => BaseType.TB_STRUCT

        case TypeBaseNode(Token(TYPE_CHAR, _), _) => BaseType.TB_CHAR

        case TypeBaseNode(Token(TYPE_INT, _), _) => BaseType.TB_INT

        case TypeBaseNode(Token(TYPE_DOUBLE, _), _) => BaseType.TB_DOUBLE

        case TypeBaseNode(Token(VOID, _), _) => BaseType.TB_VOID

        case _ => throw  RuntimeException("Unexpected type in TypeBase")

    val structName = structSymbol.map(_.symbolDef.name)

    arraySize
      .map(s => SymbolType(baseType, s.size, structName, structSymbol))
      .getOrElse(SymbolType(baseType, None, structName, structSymbol))
