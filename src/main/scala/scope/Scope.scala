package scope
import lexer.Lexer
import parser.Parser
import parser.ast.{AstNode, AstRoot, Statement}
import parser.ast.Definition.*
import parser.ast.DefinitionUtils.*
import parser.ast.Statement.*
import parser.ast.AstNode
import scope.domain.{Domain, DomainManager}
import token.{Token, TokenWithValue}
import token.TokenCode.*
import scope.symbol.{CompilerSymbol, SymbolDefinition, SymbolType}
import scope.symbol.CompilerSymbol.*
import scope.symbol.SymbolKind.*
import scope.symbol.BaseType.*
import scope.exceptions.RedefinedSymbolError

import java.io.File
import scala.collection.mutable

class Scope(ast: AstNode):

  private def checkStructIsDefined(typeBaseNode: TypeBaseNode, domainManager: DomainManager): Unit =
    typeBaseNode match
      case TypeBaseNode(Token(STRUCT, _), Some(TokenWithValue(_, line, structName: String))) =>
        if !domainManager.existsInAnyDomain(structName) then
          throw RuntimeException(s"Undefined 'struct $structName' type at line $line.")
        else ()
      case _ =>

  private def getStructSymbol(typeBase: TypeBaseNode, domainManager: DomainManager): Option[StructSymbol] =
    typeBase match
      case TypeBaseNode(Token(STRUCT, _), Some(TokenWithValue(_, _, structName: String))) =>
        domainManager.findInAnyDomain(structName) match
          case Some(struct: StructSymbol) => Option(struct)
          case _ => None
      case _ => None


  private def walkAst(ast: AstNode, domainManager: DomainManager, owner: Option[SymbolDefinition]): DomainManager =
    ast match
      case AstRoot.AstRoot(definitions*) =>
        definitions.foldLeft(domainManager)((prevDomain, nextDef) => walkAst(nextDef, prevDomain, owner))

      case StructDefNode(TokenWithValue(ID, line, structName: String), members*) =>
        if domainManager.existsInTopDomain(structName) then throw RedefinedSymbolError(structName, line)
        else
          val structDefinition = SymbolDefinition(structName, SK_STRUCT, SymbolType(TB_STRUCT), owner)
          // all struct members will be added one by one to the structs scope
          val structDomain = members.foldLeft(domainManager.pushDomain(structName))(
            (prevDomain, member) => walkAst(member, prevDomain, Option(structDefinition))
          )

          val structSymbol = StructSymbol(structDefinition, structDomain.peekDomain)

          domainManager :+ structSymbol

      case VariableDefNode(typeBase, TokenWithValue(ID, line, varName: String), arraySize) =>
        if domainManager.existsInTopDomain(varName) then throw RedefinedSymbolError(varName, line)
        else
          checkStructIsDefined(typeBase, domainManager)
          arraySize match
            case Some(ArraySizeNode(Some(0))) =>
              throw RuntimeException(s"Unspecified vector dimensions for '$varName' at line $line.")
            case _ =>

          val structSymbol = getStructSymbol(typeBase, domainManager)
          val varDefinition = SymbolDefinition(varName, SK_VAR, SymbolType(typeBase, arraySize, structSymbol), owner)

          val varSymbol =
            owner match
              case Some(SymbolDefinition(_, SK_STRUCT, _, _)) =>
                StructMemberSymbol(varDefinition, domainManager.structMemberIdx)

              case Some(SymbolDefinition(_, SK_FN, _, _)) =>
                LocalVariableSymbol(varDefinition, domainManager.localsIdx)

              case _ => GlobalVariableSymbol(varDefinition, 1)

          domainManager :+ varSymbol

      case FunctionDefNode(typeBase, TokenWithValue(ID, line, functionName: String), compoundStm, params*) =>
        if domainManager.existsInTopDomain(functionName) then throw RedefinedSymbolError(functionName, line)
        else
          checkStructIsDefined(typeBase, domainManager)

          val structSymbol = getStructSymbol(typeBase, domainManager)
          val funDefinition = SymbolDefinition(functionName, SK_FN, SymbolType(typeBase, None, structSymbol), owner)

          // all parameters will be added one by one to the functions scope
          val funDomain = params.foldLeft(domainManager.pushDomain(functionName))(
            (prevDomain, member) => walkAst(member, prevDomain, Option(funDefinition))
          )
          // all locals will be added one by one to the same function scope
          val fullFunDomain = walkAst(compoundStm, funDomain, Option(funDefinition))

          val funSymbol = FunctionSymbol(funDefinition, fullFunDomain.params, fullFunDomain.locals)

          domainManager :+ funSymbol

      case CompoundStmNode(statements*) =>
        statements.foldLeft(domainManager)((prevDomain, member) => walkAst(member, prevDomain, owner))

      case IfStmNode(_, thenBranch, elseBranch) =>
        val thenDomain = thenBranch match
          case compoundStm: CompoundStmNode =>
            walkAst(compoundStm, domainManager.pushDomain("if"), owner)
          case _ => domainManager

        elseBranch match
          case Some(compoundStm: CompoundStmNode) =>
            walkAst(compoundStm, thenDomain.pushDomain("if"), owner)
          case _ => thenDomain

      case WhileStmNode(_, body: CompoundStmNode) =>
        walkAst(body, domainManager.pushDomain("while"), owner)

      case FunctionParamNode(typeBase, TokenWithValue(ID, line, paramName: String), arraySize) =>
        if domainManager.existsInTopDomain(paramName) then throw RedefinedSymbolError(paramName, line)
        else
          checkStructIsDefined(typeBase, domainManager)

          val structSymbol = getStructSymbol(typeBase, domainManager)

          // array params are converted to pointers []
          val paramDefinition = SymbolDefinition(paramName, SK_PARAM,
            SymbolType(typeBase, arraySize.map(s => s.copy(Option(0))), structSymbol), owner)
          val paramSymbol = FunctionParameterSymbol(paramDefinition, domainManager.paramsIdx)

          domainManager :+ paramSymbol

      case _ => domainManager


  def constructDomains: DomainManager  =
    walkAst(ast, DomainManager(), None)


object Scope extends App:
  val parsedAst = Parser(Lexer(new File("src/test/testCode/testad.c")).tokenizeFile).parse
  val dd = Scope(parsedAst)
  DomainManager.pprint(dd.constructDomains)
