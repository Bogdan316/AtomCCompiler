package scope.symbol

import parser.ast.AstNode

sealed trait CompilerSymbol:
  def symbolDef: SymbolDefinition

  def size: Int = symbolDef.size

object CompilerSymbol:

  def unapply(compilerSymbol: CompilerSymbol): Some[(SymbolDefinition, Int)] =
    Some(compilerSymbol.symbolDef, compilerSymbol.size)

  final case class LocalVariableSymbol(symbolDef: SymbolDefinition, idx: Int) extends CompilerSymbol

  final case class StructMemberSymbol(symbolDef: SymbolDefinition, idx: Int) extends CompilerSymbol

  final case class GlobalVariableSymbol(symbolDef: SymbolDefinition, mem: Int) extends CompilerSymbol

  final case class FunctionParameterSymbol(symbolDef: SymbolDefinition, idx: Int) extends CompilerSymbol

  final case class StructSymbol(symbolDef: SymbolDefinition, members: List[StructMemberSymbol]) extends CompilerSymbol:
    override def size: Int = members.map(_.size).sum

  final case class FunctionSymbol(symbolDef: SymbolDefinition, params: List[CompilerSymbol], locals: List[CompilerSymbol])
    extends CompilerSymbol:
    override def size: Int = BaseType.TB_VOID_PTR.size

  def pprint(symbol: CompilerSymbol, depth: Int = 0): Unit =
    val ident = "\t" * depth
    println(s"$ident${symbol.getClass.getSimpleName}:")

    symbol match
      case StructSymbol(symbolDef, members) =>
        SymbolDefinition.pprint(symbolDef, depth + 1)
        println(s"$ident\tmembers:")
        members.foreach(m => pprint(m, depth + 2))
        println(s"$ident\tsize: ${symbol.size}")

      case LocalVariableSymbol(symbolDef, idx) =>
        SymbolDefinition.pprint(symbolDef, depth + 1)
        println(s"$ident\tidx: $idx")
        println(s"$ident\tsize: ${symbol.size}")

      case StructMemberSymbol(symbolDef, idx) =>
        SymbolDefinition.pprint(symbolDef, depth + 1)
        println(s"$ident\tidx: $idx")
        println(s"$ident\tsize: ${symbol.size}")

      case GlobalVariableSymbol(symbolDef, mem) =>
        SymbolDefinition.pprint(symbolDef, depth + 1)
        println(s"$ident\tmem: $mem")
        println(s"$ident\tsize: ${symbol.size}")

      case FunctionSymbol(symbolDef, params, locals) =>
        SymbolDefinition.pprint(symbolDef, depth + 1)
        println(s"$ident\tparams:")
        params.foreach(p => pprint(p, depth + 2))
        println(s"$ident\tlocals:")
        locals.foreach(l => pprint(l, depth + 2))

      case FunctionParameterSymbol(symbolDef, idx) =>
        SymbolDefinition.pprint(symbolDef, depth + 1)
        println(s"$ident\tidx: $idx")
        println(s"$ident\tsize: ${symbol.size}")
