package types.returntype

import scope.symbol.BaseType.*
import scope.symbol.{BaseType, SymbolType}

sealed trait ReturnType:
  def returnedType: SymbolType

  def isScalar: Boolean =
    returnedType match
      case SymbolType(_, Some(_), _) => false
      case SymbolType(BaseType.TB_VOID, _, _)  => false
      case _ => true

  def canBeConvertedTo(other: SymbolType): Boolean =
    (this.returnedType, other) match
      case (SymbolType(_, Some(s1), _), SymbolType(_, Some(s2), _)) if s1 >= 0 => s2 >= 0

      case (_, SymbolType(_, Some(s), _)) if s >= 0 => false

      case (SymbolType(TB_INT | TB_CHAR | TB_DOUBLE, _, _), SymbolType(TB_INT | TB_CHAR | TB_DOUBLE, _, _)) => true

      case (SymbolType(TB_STRUCT, _, sy1), SymbolType(TB_STRUCT, _, sy2)) => sy1 eq sy2

      case _ => false

  def canBeConvertedTo(other: ReturnType): Boolean =
    canBeConvertedTo(other.returnedType)

  def coerceTo(other: SymbolType): Option[SymbolType] =
    (this.returnedType, other) match
      case (SymbolType(_, Some(_), _), SymbolType(_, Some(_), _)) => None

      case (SymbolType(TB_INT, _, _), SymbolType(dstType, _, _)) =>
        dstType match
          case TB_INT | TB_CHAR => Some(SymbolType(TB_INT))

          case TB_DOUBLE => Some(SymbolType(TB_DOUBLE))

          case _ => None

      case (SymbolType(TB_DOUBLE, _, _), SymbolType(TB_INT | TB_CHAR | TB_DOUBLE, _, _)) => Some(SymbolType(TB_DOUBLE))

      case (SymbolType(TB_CHAR, _, _), SymbolType(dstType@(TB_INT | TB_CHAR | TB_DOUBLE), _, _)) => Some(SymbolType(dstType))

      case _ => None

  def coerceTo(other: ReturnType): Option[SymbolType] =
    coerceTo(other.returnedType)


object ReturnType:

  def unapply(r: ReturnType): Option[SymbolType] = Some(r.returnedType)

  final case class LeftSideValue(returnedType: SymbolType) extends ReturnType

  final case class AmbidexValue(returnedType: SymbolType) extends ReturnType

  final case class RightSideValue(returnedType: SymbolType) extends ReturnType
