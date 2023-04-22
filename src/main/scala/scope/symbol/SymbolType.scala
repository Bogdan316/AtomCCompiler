package scope.symbol

import parser.ast.AstNode
import parser.ast.AstNode.DefinitionUtils.{ArraySizeNode, TypeBaseNode}
import scope.domain.DomainManager
import scope.symbol.CompilerSymbol.StructSymbol
import scope.symbol.BaseType
import token.Token.TypeToken.*
import token.Token

case class SymbolType
(
  baseType: BaseType,
  arraySize: Option[Int] = None,
  structSymbol: Option[StructSymbol] = None
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
  def apply(typeBase: TypeBaseNode, arraySize: Option[ArraySizeNode], structSymbol: Option[StructSymbol]): SymbolType =
    val baseType =
      typeBase match
        case TypeBaseNode(StructTypeToken(_), _) => BaseType.TB_STRUCT

        case TypeBaseNode(CharTypeToken(_), _) => BaseType.TB_CHAR

        case TypeBaseNode(IntTypeToken(_), _) => BaseType.TB_INT

        case TypeBaseNode(DoubleTypeToken(_), _) => BaseType.TB_DOUBLE

        case TypeBaseNode(VoidTypeToken(_), _) => BaseType.TB_VOID

    arraySize
      .map(s => SymbolType(baseType, s.size, structSymbol))
      .getOrElse(SymbolType(baseType, None, structSymbol))
