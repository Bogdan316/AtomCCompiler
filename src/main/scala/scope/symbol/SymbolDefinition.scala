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
      case SymbolDefinition(name, kind, SymbolType(baseType, arraySize, structId, _), owner) =>
        println(s"$ident\tname: $name")
        println(s"$ident\tkind: $kind")
        println(s"$ident\ttype: $baseType${structId.map(id => s" $id").getOrElse("")}")
        println(s"$ident\tarraySize: ${arraySize.getOrElse("")}")
        println(s"$ident\towner: ${owner.map(_.name).getOrElse("global")}")
