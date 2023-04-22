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
import token.Token.IdentifierToken
import token.Token.LiteralToken.*
import token.Token.OperatorToken.*
import token.Token.TypeToken.*
import token.Token
import types.ReturnType
import types.ReturnType.*

import java.io.File
import scala.collection.mutable

class Scope(ast: AstNode):

  private def checkStructIsDefined(typeBaseNode: TypeBaseNode, domainManager: DomainManager): Unit =
    typeBaseNode match
      case TypeBaseNode(StructTypeToken(_), Some(IdentifierToken(line, structName))) =>
        if !domainManager.existsInAnyDomain(structName) then
          throw RuntimeException(s"Undefined 'struct $structName' type at line $line.")
        else ()
      case _ =>

  private def getStructSymbol(typeBase: TypeBaseNode, domainManager: DomainManager): Option[StructSymbol] =
    typeBase match
      case TypeBaseNode(StructTypeToken(_), Some(IdentifierToken(_, structName))) =>
        domainManager.findInAnyDomain(structName) match
          case Some(struct: StructSymbol) => Option(struct)
          case _ => None
      case _ => None

  private def walkAst(statementNode: StatementNode, domainManager: DomainManager, owner: Option[SymbolDefinition]): DomainManager =
    statementNode match
      case CompoundStmNode(statements*) =>
        statements.foldLeft(domainManager)((prevDomain, member) => walkAst(member, prevDomain, owner))

      case node@IfStmNode(_, thenBranch, elseBranch) =>
        typeCheckExpression(node, domainManager, owner)

        val thenDomain = thenBranch match
          case compoundStm: CompoundStmNode =>
            walkAst(compoundStm, domainManager.pushDomain("if"), owner)
          case _ => domainManager

        elseBranch match
          case Some(compoundStm: CompoundStmNode) =>
            walkAst(compoundStm, thenDomain.pushDomain("if"), owner)
          case _ => thenDomain

      case node@ReturnStmNode(_) =>
        typeCheckExpression(node, domainManager, owner)
        domainManager

      case node@ExpressionStmNode(_) =>
        typeCheckExpression(node, domainManager, owner)
        domainManager

      case node@WhileStmNode(_, body) =>
        typeCheckExpression(node, domainManager, owner)

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

      case FunctionDefNode(typeBase, IdentifierToken(line, functionName), compoundStm, params*) =>
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


  private def walkAst(ast: AstNode, domainManager: DomainManager, owner: Option[SymbolDefinition]): DomainManager =
    ast match
      case s: StatementNode => walkAst(s, domainManager, owner)

      case d: DefinitionNode => walkAst(d, domainManager, owner)

      case AstRoot(definitions*) =>
        definitions.foldLeft(domainManager.pushDomain("global"))((prevDomain, nextDef) => walkAst(nextDef, prevDomain, owner))

      case FunctionParamNode(typeBase, IdentifierToken(line, paramName), arraySize) =>
        if domainManager.existsInTopDomain(paramName) then throw RedefinedSymbolError(paramName, line)
        else
          checkStructIsDefined(typeBase, domainManager)

          val structSymbol = getStructSymbol(typeBase, domainManager)

          // array params are converted to pointers []
          val paramDefinition = SymbolDefinition(paramName, SK_PARAM,
            SymbolType(typeBase, arraySize.map(s => s.copy(Option(0))), structSymbol), owner)
          val paramSymbol = FunctionParameterSymbol(paramDefinition, domainManager.paramsIdx)

          domainManager :+ paramSymbol


  private def typeCheckExpression(expr: ExpressionNode, domainManager: DomainManager): ReturnType =
    expr match
      case VariableExprNode(IdentifierToken(line, varName)) =>
        domainManager.findInAnyDomain(varName) match
          case Some(_: FunctionSymbol) => throw RuntimeException("a function needs to be called")

          case Some(varSymbol@(GlobalVariableSymbol(_, _) | LocalVariableSymbol(_, _) | FunctionParameterSymbol(_, _))) =>
            varSymbol match
              case CompilerSymbol(SymbolDefinition(_, _, symbolType, _), _) =>
                symbolType match
                  case st@SymbolType(_, Some(s), _) if s >= 0 => AmbidexValue(st)
                  case st => LeftSideValue(st)

          case _ => throw RuntimeException(s"ID not found '$varName' at line $line")

      case LiteralExprNode(IntLiteralToken(_, _)) => RightSideValue(SymbolType(TB_INT))

      case LiteralExprNode(DoubleLiteralToken(_, _)) => RightSideValue(SymbolType(TB_DOUBLE))

      case LiteralExprNode(CharLiteralToken(_, _)) => RightSideValue(SymbolType(TB_CHAR))

      case LiteralExprNode(StringLiteralToken(_, _)) => RightSideValue(SymbolType(TB_CHAR, Some(0)))

      case FunctionCallExprNode(IdentifierToken(line, funName), args*) =>
        domainManager.findInAnyDomain(funName) match
          case Some(FunctionSymbol(funDef, params, _)) =>
            (params.length, args.length) match
              case (paramsLength, argsLength) if paramsLength < argsLength => throw RuntimeException("too many arguments")

              case (paramsLength, argsLength) if paramsLength > argsLength => throw RuntimeException("too few arguments")

              case _ =>
                val canBeAssigned = args.zip(params.map(_.symbolDef.symbolType)).forall(
                  pair => typeCheckExpression(pair._1, domainManager)
                    .canBeConvertedTo(pair._2)
                )
                if canBeAssigned then RightSideValue(funDef.symbolType)
                else throw RuntimeException("Cannot convert")

          case Some(_) => throw RuntimeException("only a function can be called")

          case _ => throw RuntimeException(s"undefined function at line $line")

      case ArrayAccessExprNode(arrayExpression, idxExpression) =>
        val arrExpr = typeCheckExpression(arrayExpression, domainManager)
        val idxExpr = typeCheckExpression(idxExpression, domainManager)

        arrExpr match
          case AmbidexValue(SymbolType(baseType, Some(_), structSymbol)) =>
            // index should be int
            if !idxExpr.canBeConvertedTo(SymbolType(TB_INT)) then
              throw RuntimeException("the index is not convertible to int")
            else LeftSideValue(SymbolType(baseType, None, structSymbol))
          case _ => throw RuntimeException("only an array can be indexed")

      case FieldAccessExprNode(left, field) =>
        val leftReturnType = typeCheckExpression(left, domainManager)
        leftReturnType match
          case ReturnType(SymbolType(TB_STRUCT, _, Some(structSymbol))) =>
            field match
              case IdentifierToken(_, varName) =>
                val structMember = structSymbol.members.find(_.symbolDef.name == varName)
                structMember match
                  case Some(StructMemberSymbol(SymbolDefinition(_, _, t@SymbolType(_, Some(_), _), _), _)) =>
                    AmbidexValue(t)

                  case Some(StructMemberSymbol(SymbolDefinition(_, _, t@SymbolType(_, None, _), _), _)) =>
                    LeftSideValue(t)

                  case _ => throw RuntimeException(s"the structure ${structSymbol.symbolDef.name} does not have a field $varName")

          case _ => throw RuntimeException("a field can only be selected from a struct")

      case UnaryExprNode(_, right) =>
        val rightExpr = typeCheckExpression(right, domainManager)
        if !rightExpr.isScalar then throw RuntimeException("unary - or ! must have a scalar operand")
        else RightSideValue(rightExpr.returnedType)

      case CastExprNode(typeBase, arrSizeNode, castedExpr) =>
        val exprReturnType = typeCheckExpression(castedExpr, domainManager)
        typeBase match
          case TypeBaseNode(StructTypeToken(_), _) => throw RuntimeException("cannot convert to a struct type")
          case _ =>
            exprReturnType match
              case ReturnType(SymbolType(TB_STRUCT, _, _)) => throw RuntimeException("cannot convert a struct")

              case ReturnType(SymbolType(_, castFromArrSize, _)) =>
                (arrSizeNode, castFromArrSize) match
                  case (Some(_), None) => throw RuntimeException("an array can be converted only to another array")

                  case (None, Some(_)) => throw RuntimeException("a scalar can be converted only to another scalar")

                  case _ => RightSideValue(SymbolType(typeBase, arrSizeNode, getStructSymbol(typeBase, domainManager)))

              case _ => throw RuntimeException("error during cast expression")

      case BinaryExprNode(left, operator, right) =>
        val leftReturnType = typeCheckExpression(left, domainManager)
        val rightReturnType = typeCheckExpression(right, domainManager)

        operator match
          case _: ArithmeticOperatorToken =>
            leftReturnType.coerceTo(rightReturnType) match
              case Some(dstType) => RightSideValue(dstType)
              case None => throw RuntimeException("invalid operand type for * or /")

          case _: LogicalOperatorToken =>
            leftReturnType.coerceTo(rightReturnType) match
              case Some(_) => RightSideValue(SymbolType(TB_INT))
              case None => throw RuntimeException("invalid operand type for <, <=, >, >=")

      case AssignmentExprNode(left, right) =>
        val leftReturnType = typeCheckExpression(left, domainManager)
        val rightReturnType = typeCheckExpression(right, domainManager)

        (leftReturnType, rightReturnType) match
          case (RightSideValue(_), _) => throw RuntimeException("the assign destination cannot be constant")

          case (dstType, _) if !dstType.isScalar => throw RuntimeException("the assign destination must be scalar")

          case (_, srcType) if !srcType.isScalar => throw RuntimeException("the assign source must be scalar")

          case (dstType, srcType) if !srcType.canBeConvertedTo(dstType) =>
            throw RuntimeException("the assign source cannot be converted to destination")

          case (LeftSideValue(_) | AmbidexValue(_), ReturnType(srcType)) => RightSideValue(srcType)

          case _ => throw RuntimeException("the assign destination must be a left-value")

  def typeCheckExpression(statementNode: StatementNode, domainManager: DomainManager, owner: Option[SymbolDefinition]): Unit =
    statementNode match
      case IfStmNode(condition, _, _) =>
        val conditionType = typeCheckExpression(condition, domainManager)
        if !conditionType.isScalar then throw RuntimeException("the if condition must be a scalar value")
        else ()

      case WhileStmNode(condition, _) =>
        val conditionType = typeCheckExpression(condition, domainManager)
        if !conditionType.isScalar then throw RuntimeException("the while condition must be a scalar value")
        else ()

      case ReturnStmNode(Some(expr)) =>
        owner match
          case Some(SymbolDefinition(_, _, SymbolType(TB_VOID, _, _), _)) => throw RuntimeException("a void function cannot return a value")
          case None => throw RuntimeException("Return statement should be inside a function")
          case Some(_, _, ownerType, _) =>
            val exprType = typeCheckExpression(expr, domainManager)
            if !exprType.isScalar then throw RuntimeException("the return value must be a scalar value")
            else if !exprType.canBeConvertedTo(ownerType) then
              throw RuntimeException("cannot convert the return expression type to the function return type")
            else ()

      case ReturnStmNode(None) =>
        owner match
          case Some(SymbolDefinition(_, _, SymbolType(TB_VOID, _, _), _)) => ()
          case _ => throw RuntimeException("a non-void function must return a value")

      case ExpressionStmNode(Some(expr)) => typeCheckExpression(expr, domainManager)

      case ExpressionStmNode(None) => ()

      case CompoundStmNode(_*) => ()

  def constructDomains: DomainManager =
    walkAst(ast, DomainManager(), None)


object Scope extends App :
//  TODO: separate logic
  val parsedAst = Parser(Lexer(new File("src/test/testCode/testad.c")).tokenizeFile).parse
  val dd = Scope(parsedAst)
  DomainManager.pprint(dd.constructDomains)
