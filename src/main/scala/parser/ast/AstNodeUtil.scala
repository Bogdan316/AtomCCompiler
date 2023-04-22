package parser.ast

import parser.ast.AstNode.AstRoot
import parser.ast.AstNode.AstRoot.*
import parser.ast.AstNode.DefinitionNode.*
import parser.ast.AstNode.DefinitionUtils.*
import parser.ast.AstNode.ExpressionNode.*
import parser.ast.AstNode.StatementNode.*
import token.Token.DelimiterToken.*
import token.Token.KeywordToken.*
import token.Token.LiteralToken.*
import token.Token.OperatorToken.*
import token.Token.TypeToken.*
import token.Token.{IdentifierToken, TokenWithValue}
import token.TokenUtils.stringify



object AstNodeUtil:

  def pprint(ast: AstNode, depth: Int = 0): Unit =
    val ident = "\t" * depth
    println(s"$ident${ast.getClass.getSimpleName}:")
    ast match
      case AstRoot(definitions*) =>
        definitions.foreach(d => pprint(d, depth + 1))

      case TypeBaseNode(baseType, structId) =>
        val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t)}").getOrElse("")
        print(s"$ident\t$typeBaseStr")

      case ArraySizeNode(arraySize) =>
        println(arraySize.map(s => s"${ident}arraySize: $s").getOrElse(s"${ident}arraySize: 0"))

      case VariableDefNode(baseType, id, arraySize) =>
        println(s"$ident\ttype: ")
        pprint(baseType, depth + 2)
        println(s"\n$ident\tid: ${stringify(id)}")
        arraySize match
          case Some(size) => pprint(size, depth + 1)
          case _ => ()

      case StructDefNode(id, varDefinitions*) =>
        println(s"$ident\tid: ${stringify(id)}")
        varDefinitions.foreach(d => pprint(d, depth + 1))

      case FunctionDefNode(returnType, id, stm, params*) =>
        println(s"$ident\treturnType: ")
        pprint(returnType, depth + 2)
        println(s"\n$ident\tid: ${stringify(id)}")
        params.foreach(p => pprint(p, depth + 1))
        pprint(stm, depth + 1)

      case FunctionParamNode(typeBase, id, arraySize) =>
        println(s"$ident\ttype: ")
        pprint(typeBase, depth + 2)
        println(s"\n$ident\tid: ${stringify(id)}")
        arraySize match
          case Some(size) => pprint(size, depth + 1)
          case _ => ()

      case CompoundStmNode(compoundStatements*) =>
        compoundStatements.foreach(s => pprint(s, depth + 1))

      case FunctionCallExprNode(funName, expressions*) =>
        println(s"$ident\tname: ${stringify(funName)}")
        println(s"$ident\tParams:")
        expressions.foreach(e => pprint(e, depth + 2))

      case LiteralExprNode(token) =>
        token match
          case IntLiteralToken(_, value) => println(s"$ident\tvalue: $value")
          case StringLiteralToken(_, value) => println(s"$ident\tvalue: \"$value\"")
          case CharLiteralToken(_, value) => println(s"$ident\tvalue: \'$value\'")
          case _ =>

      case BinaryExprNode(left, op, right) =>
        pprint(left, depth + 1)
        println(s"$ident\t${stringify(op)}")
        pprint(right, depth + 1)

      case UnaryExprNode(op, expr) =>
        println(s"$ident\t${stringify(op)}")
        pprint(expr, depth + 1)

      case VariableExprNode(t) =>
        println(s"$ident\tvalue: ${stringify(t)}")

      case CastExprNode(baseType, arraySize, castedExpr) =>
        print(s"$ident\tcast to: ")
        pprint(baseType)
        arraySize match
          case Some(size) => pprint(size)
          case _ => println()
        pprint(castedExpr, depth + 1)

      case IfStmNode(cond, thenBranch, elseBranch) =>
        println(s"$ident\tcond:")
        pprint(cond, depth + 2)
        println(s"$ident\tthenBranch:")
        pprint(thenBranch, depth + 2)
        elseBranch.foreach(
          e => {
            println(s"$ident\telseBranch:")
            pprint(e, depth + 2)
          }
        )

      case WhileStmNode(cond, body) =>
        println(s"$ident\tcond:")
        pprint(cond, depth + 2)
        println(s"$ident\tbody:")
        pprint(body, depth + 2)

      case ReturnStmNode(expr) =>
        expr.foreach(r => pprint(r, depth + 1))

      case ExpressionStmNode(expr) =>
        expr.foreach(e => pprint(e, depth + 1))

      case FieldAccessExprNode(expr, IdentifierToken(_, fieldName)) =>
        pprint(expr, depth + 1)
        println(s"$ident\tfieldName: $fieldName")

      case ArrayAccessExprNode(arrayExpression, idxExpression) =>
        println(s"$ident\tarrayExpression:")
        pprint(arrayExpression, depth + 2)
        println(s"$ident\tidxExpression:")
        pprint(idxExpression, depth + 2)

      case AssignmentExprNode(left, right) =>
        pprint(left, depth + 1)
        println(s"$ident\t=")
        pprint(right, depth + 1)

