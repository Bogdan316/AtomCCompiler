package parser.utils

import parser.ast.AstNode
import parser.ast.AstRoot.AstRoot
import parser.ast.Definition.*
import parser.ast.DefinitionUtils.{ArraySize, FunctionParam, TypeBase}
import parser.ast.Expression.*
import parser.ast.Statement.*
import token.Token.stringify
import token.TokenCode.{CHAR, INT, STRING}
import token.TokenWithValue

object PrettyPrint:

  def pprint(ast: AstNode, depth: Int = 0): Unit =
    val ident = "\t" * depth
    ast match
      case AstRoot(definitions*) =>
        println("AstRoot:")
        definitions.foreach(d => pprint(d, depth + 1))

      case TypeBase(baseType, structId) =>
        val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t)}").getOrElse("")
        print(s"$ident$typeBaseStr")

      case ArraySize(arraySize) =>
        println(s"${ident}arraySize: ${stringify(arraySize)}")

      case VariableDef(baseType, id, arraySize) =>
        println(s"${ident}VariableDef:")
        print(s"$ident\ttype: ")
        pprint(baseType)
        println(s"\n$ident\tid: ${stringify(id)}")
        arraySize match
          case Some(size) => pprint(size, depth + 1)
          case _ => ()

      case StructDef(id, varDefinitions*) =>
        println(s"${ident}StructDef:")
        println(s"$ident\tid: ${stringify(id)}")
        varDefinitions.foreach(d => pprint(d, depth + 1))

      case FunctionDef(returnType, id, stm, params*) =>
        println(s"${ident}FunctionDef:")
        print(s"$ident\treturnType: ")
        pprint(returnType)
        println(s"\n$ident\tid: ${stringify(id)}")
        params.foreach(p => pprint(p, depth + 1))
        pprint(stm, depth + 1)

      case FunctionParam(typeBase, id, arraySize) =>
        println(s"${ident}FunctionParam:")
        print(s"$ident\ttype: ")
        pprint(typeBase)
        println(s"\n$ident\tid: ${stringify(id)}")
        arraySize match
          case Some(size) => pprint(size, depth + 1)
          case _ => ()

      case CompoundStm(compoundStatements*) =>
        println(s"${ident}CompoundStm:")
        compoundStatements.foreach(s => pprint(s, depth + 1))

      case FunctionCallExpr(funName, expressions*) =>
        println(s"${ident}FunctionCallExpr:")
        println(s"$ident\tname: ${stringify(funName)}")
        println(s"$ident\tParams:")
        expressions.foreach(e => pprint(e, depth + 2))

      case LiteralExpr(TokenWithValue(code, _, value)) =>
        println(s"${ident}LiteralExpr:")
        code match
          case INT => println(s"$ident\tvalue: $value")
          case STRING => println(s"$ident\tvalue: \"$value\"")
          case CHAR => println(s"$ident\tvalue: \'$value\'")
          case _ =>

      case BinaryExpr(left, op, right) =>
        println(s"${ident}BinaryExpr:")
        pprint(left, depth + 1)
        println(s"$ident\t${op.tokenCode}")
        pprint(right, depth + 1)

      case UnaryExpr(op, expr) =>
        println(s"${ident}UnaryExpr:")
        println(s"$ident\t${op.tokenCode}")
        pprint(expr, depth + 1)

      case VariableExpr(t) =>
        println(s"${ident}VariableExpr:")
        println(s"$ident\tvalue: ${stringify(t)}")

      case CastExpr(baseType, arraySize, castedExpr) =>
        println(s"${ident}CastExpr:")
        print(s"$ident\tcast to: ")
        pprint(baseType)
        arraySize match
          case Some(size) => pprint(size)
          case _ => println()
        pprint(castedExpr, depth + 1)

      case IfStm(cond, thenBranch, elseBranch) =>
        println(s"${ident}IfStm:")
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

      case WhileStm(cond, body) =>
        println(s"${ident}WhileStm:")
        println(s"$ident\tcond:")
        pprint(cond, depth + 2)
        println(s"$ident\tbody:")
        pprint(body, depth + 2)

      case ReturnStm(expr) =>
        println(s"${ident}ReturnStm:")
        expr.foreach(r => pprint(r, depth + 1))

      case ExpressionStm(expr) =>
        println(s"${ident}ExpressionStm:")
        expr.foreach(e => pprint(e, depth + 1))

      case _ =>