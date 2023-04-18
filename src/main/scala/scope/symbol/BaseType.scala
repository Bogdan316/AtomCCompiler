package scope.symbol

sealed trait BaseType:
  def size: Int

object BaseType:
  case object TB_INT extends BaseType { val size = 4 }

  case object TB_DOUBLE extends BaseType { val size = 8 }

  case object TB_CHAR extends BaseType { val size = 1 }

  case object TB_VOID extends BaseType { val size = 1 }

  case object TB_STRUCT extends BaseType { val size: Int = -1 }

  case object TB_VOID_PTR extends BaseType { val size = 8 }
