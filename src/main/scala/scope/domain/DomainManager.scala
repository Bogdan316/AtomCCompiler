package scope.domain

import parser.ast.AstNode.DefinitionUtils.TypeBaseNode
import scope.symbol.CompilerSymbol
import scope.symbol.CompilerSymbol.{FunctionParameterSymbol, LocalVariableSymbol, StructSymbol}
import token.Token.IdentifierToken
import token.Token.TypeToken.StructTypeToken

import scala.annotation.targetName

/**
 * Acts as a stack that manages all the active scopes (domains), the head is the current scope.
 * @param domains list with all the active scopes
 */
case class DomainManager(domains: List[Domain] = List()):

  def pushDomain(domainName: String): DomainManager =
    domains.headOption match
      // by copying the top domain the indexes continue to be updated across domains
      case Some(parentDomain) => DomainManager(parentDomain.copy(domainName, List()) +: domains)
      case _ => DomainManager(Domain(domainName) +: domains)

  def peekDomain: List[CompilerSymbol] = domains.headOption.map(_.symbols).getOrElse(List())

  def structMemberIdx: Int = domains.headOption.map(_.structMemberIdx).getOrElse(0)

  def localsIdx: Int = domains.headOption.map(_.localsIdx).getOrElse(0)

  def paramsIdx: Int = domains.headOption.map(_.paramsIdx).getOrElse(0)

  def existsInTopDomain(symbolName: String): Boolean = domains.headOption.exists(_.contains(symbolName))

  def existsInAnyDomain(symbolName: String): Boolean = domains.exists(_.contains(symbolName))

  def findInAnyDomain(symbolName: String): Option[CompilerSymbol] =
    (for {
        domain <- domains
        symbol <- domain.find(symbolName)
      } yield symbol).headOption

  def locals: List[LocalVariableSymbol] =
    for {
      // skip the global domain (last domain), it cannot contain locals or params
      domain <- domains.reverse.tail
      local <- domain.locals
    } yield local

  def params: List[FunctionParameterSymbol] =
    for {
      // skip the global domain (last domain), it cannot contain locals or params
      domain <- domains.reverse.tail
      param <- domain.params
    } yield param

  @targetName("addSymbolToTopDomain")
  def :+(symbol: CompilerSymbol): DomainManager =
    domains.headOption
      .map(d => DomainManager((d :+ symbol) :: domains.tail))
      .getOrElse(DomainManager(Domain("global", List(symbol)) :: Nil))

  def getStructSymbol(typeBase: TypeBaseNode): Option[StructSymbol] =
    typeBase match
      case TypeBaseNode(StructTypeToken(_), Some(IdentifierToken(_, structName))) =>
        this.findInAnyDomain(structName) match
          case Some(struct: StructSymbol) => Option(struct)
          case _ => None
      case _ => None

  def checkStructIsDefined(typeBaseNode: TypeBaseNode): Unit =
    typeBaseNode match
      case TypeBaseNode(StructTypeToken(_), Some(IdentifierToken(line, structName))) =>
        if !this.existsInAnyDomain(structName) then
          throw RuntimeException(s"Undefined 'struct $structName' type at line $line.")
        else ()
      case _ =>

case object DomainManager:

  def pprint(domainManager: DomainManager, depth: Int = 0): Unit =
    val ident = "\t" * depth
    println(s"${ident}${domainManager.getClass.getSimpleName}:")
    domainManager.domains.foreach(d => Domain.pprint(d, depth + 1))
