package scope.symbol

trait BaseType(val size: Int)

object BaseType:

  case object TB_INT extends BaseType(4)

  case object TB_DOUBLE extends BaseType(8)

  case object TB_CHAR extends BaseType(1)

  case object TB_VOID extends BaseType(1)

  case object TB_STRUCT extends BaseType(-1)

  case object TB_VOID_PTR extends BaseType(8)
