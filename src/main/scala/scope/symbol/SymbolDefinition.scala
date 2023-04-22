package scope.symbol

case class SymbolDefinition
(
  name: String,
  kind: SymbolKind,
  symbolType: SymbolType,
  owner: Option[SymbolDefinition]
):
  def size: Int = symbolType.size


case object SymbolDefinition:

  def pprint(symbolDef: SymbolDefinition, depth: Int = 0): Unit =
    val ident = "\t" * depth
    println(s"${ident}${symbolDef.getClass.getSimpleName}:")
    symbolDef match
      case SymbolDefinition(name, kind, SymbolType(baseType, arraySize, structSymbol), owner) =>
        println(s"$ident\tname: $name")
        println(s"$ident\tkind: $kind")
        println(s"$ident\ttype: $baseType${structSymbol.map(sy => s" ${sy.symbolDef.name}").getOrElse("")}")
        println(s"$ident\tarraySize: ${arraySize.getOrElse("")}")
        println(s"$ident\towner: ${owner.map(_.name).getOrElse("global")}")
