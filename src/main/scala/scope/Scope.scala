package scope

import lexer.Lexer
import parser.Parser
import parser.ast.AstNode.*
import parser.ast.AstNode.DefinitionNode.*
import parser.ast.AstNode.DefinitionUtils.*
import parser.ast.AstNode.ExpressionNode.*
import parser.ast.AstNode.StatementNode.*
import parser.ast.{AstNode, AstNodeUtil}
import scope.domain.{Domain, DomainManager}
import scope.exceptions.RedefinedSymbolError
import scope.symbol.BaseType.*
import scope.symbol.CompilerSymbol.*
import scope.symbol.SymbolKind.*
import scope.symbol.{CompilerSymbol, SymbolDefinition, SymbolType}
import token.Token
import token.Token.IdentifierToken
import token.Token.LiteralToken.*
import token.Token.OperatorToken.*
import token.Token.TypeToken.*
import types.TypeCheck.typeCheckExpression
import types.returntype.ReturnType
import types.returntype.ReturnType.*

import java.io.File
import scala.collection.mutable

class Scope(ast: AstNode):

  private def walkAst(statementNode: StatementNode, domainManager: DomainManager, owner: Option[SymbolDefinition]): DomainManager =
    statementNode match
      case CompoundStmNode(statements*) =>
        statements.foldLeft(domainManager)((prevDomain, member) => walkAst(member, prevDomain, owner))

      case IfStmNode(_, thenBranch, elseBranch) =>
        typeCheckExpression(statementNode, domainManager, owner)

        val thenDomain = thenBranch match
          case compoundStm: CompoundStmNode =>
            walkAst(compoundStm, domainManager.pushDomain("if then"), owner)
          case _ => domainManager

        elseBranch match
          case Some(compoundStm: CompoundStmNode) =>
            walkAst(compoundStm, thenDomain.pushDomain("if else"), owner)
          case _ => thenDomain

      case ReturnStmNode(_) =>
        typeCheckExpression(statementNode, domainManager, owner)
        domainManager

      case ExpressionStmNode(_) =>
        typeCheckExpression(statementNode, domainManager, owner)
        domainManager

      case WhileStmNode(_, body) =>
        typeCheckExpression(statementNode, domainManager, owner)

        walkAst(body, domainManager.pushDomain("while"), owner)


  private def walkAst(definitionNode: DefinitionNode, domainManager: DomainManager, owner: Option[SymbolDefinition]): DomainManager =
    definitionNode match
      case StructDefNode(IdentifierToken(line, structName), members*) =>
        if domainManager.existsInTopDomain(structName) then throw RedefinedSymbolError(structName, line)
        else
          val structDefinition = SymbolDefinition(structName, SK_STRUCT, SymbolType(TB_STRUCT), owner)
          // all struct members will be added one by one to the structs scope
          val structDomain = members.foldLeft(domainManager.pushDomain(structName))(
            (prevDomain, member) => walkAst(member, prevDomain, Option(structDefinition))
          )

          val structSymbol = StructSymbol(structDefinition, structDomain.peekDomain.asInstanceOf[List[StructMemberSymbol]])

          domainManager :+ structSymbol

      case VariableDefNode(typeBase, IdentifierToken(line, varName), arraySize) =>
        if domainManager.existsInTopDomain(varName) then throw RedefinedSymbolError(varName, line)
        else
          domainManager.checkStructIsDefined(typeBase)
          arraySize match
            case Some(ArraySizeNode(Some(0))) =>
              throw RuntimeException(s"Unspecified vector dimensions for '$varName' at line $line.")
            case _ =>

          val structSymbol = domainManager.getStructSymbol(typeBase)
          val varDefinition = SymbolDefinition(varName, SK_VAR, SymbolType(typeBase, arraySize, structSymbol), owner)

          val varSymbol =
            owner match
              case Some(SymbolDefinition(_, SK_STRUCT, _, _)) =>
                StructMemberSymbol(varDefinition, domainManager.structMemberIdx)

              case Some(SymbolDefinition(_, SK_FN, _, _)) =>
                LocalVariableSymbol(varDefinition, domainManager.localsIdx)

              case _ => GlobalVariableSymbol(varDefinition, 1) // TODO: what should the address be?

          domainManager :+ varSymbol

      case FunctionDefNode(typeBase, IdentifierToken(line, functionName), compoundStm, params*) =>
        if domainManager.existsInTopDomain(functionName) then throw RedefinedSymbolError(functionName, line)
        else
          domainManager.checkStructIsDefined(typeBase)

          val structSymbol = domainManager.getStructSymbol(typeBase)
          val funDefinition = SymbolDefinition(functionName, SK_FN, SymbolType(typeBase, None, structSymbol), owner)

          // all parameters will be added one by one to the functions scope
          val funDomain = params.foldLeft(domainManager.pushDomain(functionName))(
            (prevDomain, member) => walkAst(member, prevDomain, Option(funDefinition))
          )
          // all locals will be added one by one to the same function scope
          val fullFunDomain = walkAst(compoundStm, funDomain, Option(funDefinition))

          val funSymbol = FunctionSymbol(funDefinition, fullFunDomain.params, fullFunDomain.locals)

          domainManager :+ funSymbol


  private def walkAst(ast: AstNode, domainManager: DomainManager, owner: Option[SymbolDefinition]): DomainManager =
    ast match
      case s: StatementNode => walkAst(s, domainManager, owner)

      case d: DefinitionNode => walkAst(d, domainManager, owner)

      case AstRoot(definitions*) =>
        definitions.foldLeft(domainManager.pushDomain("global"))(
          (prevDomain, nextDef) => walkAst(nextDef, prevDomain, owner)
        )

      case FunctionParamNode(typeBase, IdentifierToken(line, paramName), arraySize) =>
        if domainManager.existsInTopDomain(paramName) then throw RedefinedSymbolError(paramName, line)
        else
          domainManager.checkStructIsDefined(typeBase)

          val structSymbol = domainManager.getStructSymbol(typeBase)

          // array params are converted to pointers []
          val paramDefinition = SymbolDefinition(paramName, SK_PARAM,
            SymbolType(typeBase, arraySize.map(s => s.copy(Option(0))), structSymbol), owner)
          val paramSymbol = FunctionParameterSymbol(paramDefinition, domainManager.paramsIdx)

          domainManager :+ paramSymbol

  def constructDomains: DomainManager =
    walkAst(ast, DomainManager(), None)


object Scope extends App :
  val parsedAst = Parser(Lexer(new File("src/test/testCode/testat.c")).tokenizeFile).parse
  val dd = Scope(parsedAst)
  DomainManager.pprint(dd.constructDomains)
