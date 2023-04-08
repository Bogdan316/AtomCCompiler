package parser.utils

import parser.ast.AstNode
import parser.ast.AstRoot.UnitRule
import parser.ast.Definition.*
import parser.ast.Aux.{FunctionParam, TypeBase}
import parser.ast.Expression.*
import parser.ast.Statement.*
import token.Token.stringify
import token.TokenCode.{CHAR, INT, STRING}
import token.TokenWithValue

object PrettyPrint:

  def pprint(ast: AstNode, depth: Int = 0): Unit =
    val ident = "\t" * depth
    ast match
      case UnitRule(definitions*) =>
        println("UnitRule:")
        definitions.foreach(d => pprint(d, depth + 1))

      case TypeBase(baseType, structId) =>
        val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t).trim}").getOrElse("")
        print(s"${ident}$typeBaseStr")

      case VariableDef(baseType, id, arrayDecl) =>
        println(s"${ident}VarDef:")
        pprint(baseType, depth + 1)
        print(s" ${stringify(id)}")
        print(arrayDecl.map(d => s"[${stringify(d.size).trim}]\n").getOrElse("\n"))

      case StructDef(id, varDefs*) =>
        println(s"${ident}StructDef:")
        ident + stringify(id).trim + varDefs.foreach(d => pprint(d, depth + 1))

      case FunctionDef(TypeBase(baseType, structId), id, stm, params*) =>
        val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t).trim}").getOrElse("")
        println(s"${ident}FnDef:")
        println(s"$ident\t$typeBaseStr ${stringify(id).trim}")
        params.foreach(p => pprint(p, depth + 1))
        pprint(stm, depth + 1)

      case FunctionParam(TypeBase(baseType, structId), id, arrayDecl) =>
        val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t).trim}").getOrElse("")
        println(s"${ident}FnParam:\n$ident\t" + typeBaseStr + " " + stringify(id) +
          arrayDecl.map(d => s"[${stringify(d.size).trim}]").getOrElse(""))

      case CompoundStm(stms*) =>
        println(s"${ident}StmCompound:")
        stms.foreach(s => pprint(s, depth + 1))

      case FunctionCallExpr(funName, exprs*) =>
        println(s"${ident}FunctionCall:")
        println(s"$ident\tname: ${stringify(funName)}")
        println(s"$ident\tParams:")
        exprs.foreach(e => pprint(e, depth + 2))

      case LiteralExpr(TokenWithValue(code, _, value)) =>
        println(s"${ident}Literal:")
        code match
          case INT => println(s"$ident\tvalue: $value")
          case STRING => println(s"$ident\tvalue: \"$value\"")
          case CHAR => println(s"$ident\tvalue: \'$value\'")
          case _ =>

      case BinaryExpr(left, op, right) =>
        println(s"${ident}Binary:")
        pprint(left, depth + 1)
        println(s"$ident\t${op.tokenCode}")
        pprint(right, depth + 1)

      case UnaryExpr(op, expr) =>
        println(s"${ident}Unary:")
        println(s"$ident\t${op.tokenCode}")
        pprint(expr, depth + 1)

      case VariableExpr(t) =>
        println(s"${ident}Variable:")
        println(s"$ident\tvalue: ${stringify(t)}")

      case CastExpr(baseType, arrayDecl, castedExpr) =>
        println(s"${ident}Cast:")
        print(s"${ident}\tcast to: ")
        pprint(baseType)
        print(arrayDecl.map(d => s"[${stringify(d.size).trim}]\n").getOrElse("\n"))
        pprint(castedExpr, depth + 1)

      case IfStm(cond, thenBranch, elseBranch) =>
        println(s"${ident}If:")
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
        println(s"${ident}While:")
        println(s"$ident\tcond:")
        pprint(cond, depth + 2)
        println(s"$ident\tbody:")
        pprint(body, depth + 2)

      case ReturnStm(expr) =>
        println(s"${ident}Return:")
        expr.foreach(r => pprint(r, depth + 1))

      case ExpressionStm(expr) =>
        println(s"${ident}ExpressionStatement:")
        expr.foreach(e => pprint(e, depth + 1))

      case _ =>