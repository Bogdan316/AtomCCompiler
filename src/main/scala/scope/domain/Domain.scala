package scope.domain

import scope.symbol.CompilerSymbol.{FunctionParameterSymbol, LocalVariableSymbol, StructMemberSymbol}
import scope.symbol.{CompilerSymbol, SymbolType}

import scala.annotation.targetName

/**
 * Holds a list with the symbols from the current scope, counts function parameters, struct members and local variables
 * in the current domain.
 * @param name domain name
 * @param symbols symbols defined in the current domain
 * @param localsIdx index of the current local variable
 * @param structMemberIdx index of the current struct member
 * @param paramsIdx index of the current parameter
 */
case class Domain
(
  name: String,
  symbols: List[CompilerSymbol] = List(),
  localsIdx: Int = 0,
  structMemberIdx: Int = 0,
  paramsIdx: Int = 0
):

  def contains(symbolName: String): Boolean = symbols.exists(_.symbolDef.name == symbolName)

  def find(symbolName: String): Option[CompilerSymbol] = symbols.find(_.symbolDef.name == symbolName)

  @targetName("addSymbolToDomain")
  def :+(symbol: CompilerSymbol): Domain =
    symbol match
      case s: StructMemberSymbol => this.copy(symbols = symbols :+ s, structMemberIdx = structMemberIdx + s.size)

      case s: LocalVariableSymbol => this.copy(symbols = symbols :+ s, localsIdx = localsIdx + 1)

      case s: FunctionParameterSymbol => this.copy(symbols = symbols :+ s, paramsIdx = paramsIdx + 1)

      case s => this.copy(symbols = symbols :+ s)

  def locals: List[LocalVariableSymbol] = symbols.collect{case l: LocalVariableSymbol => l}

  def params: List[FunctionParameterSymbol] = symbols.collect{case p: FunctionParameterSymbol => p}

case object Domain:

  def pprint(domain: Domain, depth: Int = 0): Unit =
    val ident = "\t" * depth
    println(s"${ident}${domain.getClass.getSimpleName}:")
    domain.symbols.foreach(s => CompilerSymbol.pprint(s, depth + 1))
