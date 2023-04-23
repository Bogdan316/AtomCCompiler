package types

import parser.ast.AstNode.DefinitionUtils.TypeBaseNode
import parser.ast.AstNode.ExpressionNode.*
import parser.ast.AstNode.StatementNode.*
import parser.ast.AstNode.{ExpressionNode, StatementNode}
import scope.domain.DomainManager
import scope.symbol.BaseType.*
import scope.symbol.CompilerSymbol.*
import scope.symbol.{CompilerSymbol, SymbolDefinition, SymbolType}
import token.Token.IdentifierToken
import token.Token.LiteralToken.{CharLiteralToken, DoubleLiteralToken, IntLiteralToken, StringLiteralToken}
import token.Token.OperatorToken.{ArithmeticOperatorToken, LogicalOperatorToken}
import token.Token.TypeToken.StructTypeToken
import types.returntype.ReturnType
import types.returntype.ReturnType.{AmbidexValue, LeftSideValue, RightSideValue}
import types.exceptions.TypeCheckError

object TypeCheck:

  private def typeCheckExpression(expr: ExpressionNode, domainManager: DomainManager): ReturnType =
    expr match
      case VariableExprNode(IdentifierToken(line, varName)) =>
        domainManager.findInAnyDomain(varName) match
          case Some(_: FunctionSymbol) => 
            throw TypeCheckError(s"The function '$varName' is not called", line)

          case Some(varSymbol@(GlobalVariableSymbol(_, _) | LocalVariableSymbol(_, _) | FunctionParameterSymbol(_, _))) =>
            varSymbol match
              case CompilerSymbol(SymbolDefinition(_, _, symbolType, _), _) =>
                symbolType match
                  case SymbolType(_, Some(_), _) => AmbidexValue(symbolType)
                  case _ => LeftSideValue(symbolType)

          case _ => throw TypeCheckError(s"Variable name '$varName' is undefined", line)

      case LiteralExprNode(IntLiteralToken(_, _)) => RightSideValue(SymbolType(TB_INT))

      case LiteralExprNode(DoubleLiteralToken(_, _)) => RightSideValue(SymbolType(TB_DOUBLE))

      case LiteralExprNode(CharLiteralToken(_, _)) => RightSideValue(SymbolType(TB_CHAR))

      case LiteralExprNode(StringLiteralToken(_, _)) => RightSideValue(SymbolType(TB_CHAR, Some(0)))

      case FunctionCallExprNode(IdentifierToken(line, funName), args*) =>
        domainManager.findInAnyDomain(funName) match
          case Some(FunctionSymbol(funDef, params, _)) =>
            (params.length, args.length) match
              case (paramsLength, argsLength) if paramsLength < argsLength =>
                throw TypeCheckError(s"Too many arguments for function call of '$funName' " +
                  s"(expected $paramsLength got $argsLength)", line)

              case (paramsLength, argsLength) if paramsLength > argsLength =>
                throw TypeCheckError(s"Too few arguments for function call of '$funName' " +
                  s"(expected $paramsLength got $argsLength)", line)

              case _ =>
                // check if the expressions given as arguments can be converted to the parameter type
                val canBeAssigned = args.zip(params.map(_.symbolDef.symbolType)).forall(
                  pair => typeCheckExpression(pair._1, domainManager)
                    .canBeConvertedTo(pair._2)
                )
                if canBeAssigned then RightSideValue(funDef.symbolType)
                else throw TypeCheckError(s"Mismatch between arguments types and parameters types for " +
                  s"function call of $funName", line)

          case Some(_) => throw TypeCheckError(s"'$funName' is not a function", line)

          case _ => throw TypeCheckError(s"Undefined function at line '$funName'", line)

      case ArrayAccessExprNode(arrayExpression, idxExpression) =>
        val arrExpr = typeCheckExpression(arrayExpression, domainManager)
        val idxExpr = typeCheckExpression(idxExpression, domainManager)

        arrExpr match
          case AmbidexValue(SymbolType(baseType, Some(_), structSymbol)) =>
            // index should be int
            if !idxExpr.canBeConvertedTo(SymbolType(TB_INT)) then
              throw RuntimeException("The provided index is not an integer.")
            else LeftSideValue(SymbolType(baseType, None, structSymbol))
          case _ => throw RuntimeException("Only an array can be indexed")

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

                  case _ =>
                    throw RuntimeException(s"the structure ${structSymbol.symbolDef.name} does not have a field $varName")

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

                  case _ => RightSideValue(SymbolType(typeBase, arrSizeNode, domainManager.getStructSymbol(typeBase)))

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
          case Some(SymbolDefinition(_, _, SymbolType(TB_VOID, _, _), _)) =>
            throw RuntimeException("a void function cannot return a value")

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
