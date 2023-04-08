import Aux.FnParam
import lexer.Lexer
import parser.{Parser, parsed}
import parser.helpers.{ParsedTokens, Tokens}
import parser.parsed.{IsParsed, NotParsed}
import token.{Token, TokenCode, TokenWithValue, NoValueToken}
import TokenCode.*
import Token.stringify

type ParsingPair[T] = (Option[T], ParsedTokens)

import java.io.File
import scala.annotation.tailrec

trait AstNode
trait Statement extends AstNode
trait Expr extends AstNode
trait Aux extends AstNode

case class UnitRule(definitions: Statement*) extends AstNode

object Statement:
  case class StructDef(id: Token, varDefs: VarDef*) extends Statement
  case class VarDef(typeBase: Aux.TypeBase, id: Token, arrayDecl: Option[Aux.ArrayDecl] = None) extends Statement
  case class FnDef(typeBase: Aux.TypeBase, id: Token, stmCompound: StmCompound, fnParams: Aux.FnParam*) extends Statement
  case class StmCompound(stms: Statement*) extends Statement
  case class If(condition: Expr, thenBranch: Statement, elseBranch: Option[Statement] = None) extends Statement
  case class While(condition: Expr, body: Statement) extends Statement
  case class Return(expr: Option[Expr] = None) extends Statement
  case class ExpressionStatement(expr: Option[Expr] = None) extends Statement

object Expr:
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr
  case class FunctionCall(funName: Token, exprs: Expr*) extends Expr
  case class Literal[T](literal: TokenWithValue[T]) extends Expr
  case class Variable[T](variable: TokenWithValue[T]) extends  Expr
  case class Cast(typeBase: Aux.TypeBase, arrayDecl: Option[Aux.ArrayDecl] = None, castedExpr: Expr) extends Expr

object Aux:
  case class TypeBase(baseType: Token, structId: Option[Token] = None) extends Aux
  case class ArrayDecl(arrSize: Token = TokenWithValue(ID, -1, 0)) extends Aux
  case class FnParam(typeBase: TypeBase, id: Token, arrayDecl: Option[ArrayDecl] = None) extends Aux


def unit(tokens: Tokens): ParsingPair[UnitRule] =
  @tailrec
//  this needs to return the remaining tokens or just check for end
  def unitHelper(tokens: Tokens, defs: List[Statement]): List[Statement] =
    varDef(tokens) match
      case (Some(varDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ varDefinition)
      case _ =>
        structDef(tokens) match
          case (Some(structDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ structDefinition)
          case _ => fnDef(tokens) match
            case (Some(functionDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ functionDefinition)
            case (None, NotParsed(Token(END, _) :: Nil)) => defs
            case _ => throw RuntimeException("Big news")

  (Option(UnitRule(unitHelper(tokens, List())*)), IsParsed(tokens))

def structDef(tokens: Tokens): ParsingPair[Statement.StructDef] =
  tokens match
    case Token(STRUCT, _) :: (structId@Token(ID, _)) :: Token(LACC, _) :: remainingTokens =>
      varDef_*(remainingTokens) match
        case (Some(definitions), IsParsed(Token(RACC, _) :: Token(SEMICOLON, _) :: remainingTokens))  =>
          (Option(Statement.StructDef(structId, definitions*)), IsParsed(remainingTokens))
        case _ => (None, NotParsed(tokens))

    case _ => (None, NotParsed(tokens))

def varDef(tokens: Tokens): ParsingPair[Statement.VarDef] =
  typeBase(tokens) match
    case (Some(varType), IsParsed((varId@Token(ID, _)) :: Token(SEMICOLON, _) :: remainingTokens)) =>
      (Option(Statement.VarDef(varType, varId)), IsParsed(remainingTokens))

    case (Some(varType), IsParsed((varId@Token(ID, _)) :: remainingTokens)) =>
      arrayDecl(remainingTokens) match
        case (arrDecl, IsParsed(Token(SEMICOLON, _) :: remainingTokens)) =>
          (Some(Statement.VarDef(varType, varId, arrDecl)), IsParsed(remainingTokens))
        case _ => (None, NotParsed(tokens))

    case _ => (None, NotParsed(tokens))

def typeBase(tokens: Tokens): ParsingPair[Aux.TypeBase] =
  tokens match
    case (definedType@Token(TYPE_INT | TYPE_DOUBLE | TYPE_CHAR, _)) :: tail =>
      (Option(Aux.TypeBase(definedType)), IsParsed(tail))
    case (structType@Token(STRUCT, _)) :: (structId@Token(ID, _)) :: tail =>
      (Option(Aux.TypeBase(structType, Option(structId))), IsParsed(tail))
    case _ => (None, NotParsed(tokens))

def arrayDecl(tokens: Tokens): ParsingPair[Aux.ArrayDecl] =
  tokens match
    case Token(LBRACKET, _) :: Token(RBRACKET, _) :: tail => (Option(Aux.ArrayDecl()), IsParsed(tail))

    case Token(LBRACKET, _) ::  (arrSize@Token(INT, _)) :: Token(RBRACKET, _) :: tail =>
      (Option(Aux.ArrayDecl(arrSize)), IsParsed(tail))

    case _ => (None, NotParsed(tokens))

def fnParam(tokens: Tokens): ParsingPair[Aux.FnParam] =
  typeBase(tokens) match
    case (Some(paramType), IsParsed((paramId@Token(ID, _)) :: remainingTokens)) =>
      arrayDecl(remainingTokens) match
        case (arrayDecl, remainingTokens@IsParsed(_)) => (Option(Aux.FnParam(paramType, paramId, arrayDecl)), remainingTokens)
        case _ => (Option(Aux.FnParam(paramType, paramId)), IsParsed(remainingTokens))

    case _ => (None, NotParsed(tokens))

def fnDef(tokens: Tokens): ParsingPair[Statement.FnDef] =
  val (returnType, remainingTokens) =
    tokens match
      case (retType@Token(VOID, _)) :: remainingTokens => (Option(Aux.TypeBase(retType)), IsParsed(remainingTokens))
      case _ =>
        typeBase(tokens) match
          case typeB @ (Some(_), IsParsed(_)) => typeB
          case _ => (None, NotParsed(tokens))

  remainingTokens match
    case IsParsed((fnName@Token(ID, _)):: Token(LPAR, _) :: remainingTokens) =>
      fnParams(remainingTokens) match
        case (Some(params), IsParsed(Token(RPAR, _) :: remainingTokens)) =>
          stmCompound(remainingTokens) match
            case (Some(statement), remainingTokens) => (Option(Statement.FnDef(returnType.get, fnName, statement, params*)), remainingTokens)
            case _ => (None, NotParsed(tokens))
        case _ => (None, NotParsed(tokens))
    case _ => (None, NotParsed(tokens))

def fnParams(tokens: Tokens): ParsingPair[List[Aux.FnParam]] =
  @tailrec
  def fnParamsHelper(tokens: Tokens, params: List[Aux.FnParam]): ParsingPair[List[Aux.FnParam]] =
    tokens match
      case Token(COMMA, _) :: remainingTokens =>
        fnParam(remainingTokens) match
          case (Some(param), IsParsed(remainingTokens)) => fnParamsHelper(remainingTokens, params :+ param)
          case _ => (Some(List()), IsParsed(tokens))
      case _ => (Some(params), IsParsed(tokens))


  fnParam(tokens) match
    case (Some(param), IsParsed(remainingTokens)) =>
      fnParamsHelper(remainingTokens, List()) match
        case (Some(params), remainingTokens@IsParsed(_)) => (Option(param +: params), remainingTokens)
        case _ => (Some(List(param)), IsParsed(remainingTokens))

    case _ => (Some(List()), IsParsed(tokens))

def stm(tokens: Tokens): ParsingPair[Statement] =
  tokens match
    case Token(IF, _) :: Token(LPAR, _) :: tail =>
      expr(tail) match
        case (Some(ifExp),  IsParsed(Token(RPAR, _) :: tail)) =>
          stm(tail) match
            case (Some(thenBranch), IsParsed(Token(ELSE, _) :: tail)) =>
              stm(tail) match
                case (elseBranch@Some(_), remainingTokens@IsParsed(_)) =>
                  (Some(Statement.If(ifExp, thenBranch, elseBranch)), remainingTokens)
                case _ => (None, NotParsed(tokens))
            case (Some(thenBranch), remainingTokens@IsParsed(_)) =>
              (Some(Statement.If(ifExp, thenBranch)), remainingTokens)
            case _ => (None, NotParsed(tokens))
        case _ => (None, NotParsed(tokens))

    case Token(WHILE, _) :: Token(LPAR, _) :: tail =>
      expr(tail) match
        case (Some(cond), IsParsed(Token(RPAR, _) :: tail)) =>
          stm(tail) match
            case (Some(body), remainingTokens@IsParsed(_)) =>
              (Some(Statement.While(cond, body)), remainingTokens)
            case _ => (None, NotParsed(tokens))
        case _ => (None, NotParsed(tokens))

    case Token(RETURN, _) :: Token(SEMICOLON, _) :: tail => (Some(Statement.Return()), IsParsed(tail))

    case Token(RETURN, _) :: tail =>
      expr(tail) match
        case (returnExpr@Some(_), IsParsed(Token(SEMICOLON, _) :: tail)) =>
          (Some(Statement.Return(returnExpr)), IsParsed(tail))
        case _ => (None, NotParsed(tokens))

    case Token(SEMICOLON, _) :: tail => (Some(Statement.ExpressionStatement()), IsParsed(tail))

    case Token(LACC, _) :: _ => stmCompound(tokens)

    case t =>
      expr(t) match
        case (expr@Some(_), IsParsed(Token(SEMICOLON, _) :: tail)) =>
          (Some(Statement.ExpressionStatement(expr)), IsParsed(tail))
        case _ => (None, NotParsed(tokens))


def expr(tokens: Tokens): ParsingPair[Expr] =
  exprAssign(tokens)

def exprAssign(tokens: Tokens): ParsingPair[Expr] =
  exprUnary(tokens) match
    case (Some(unary), IsParsed((op@Token(ASSIGN, _)) :: tail)) =>
      exprAssign(tail) match
        case (Some(assign), remainingTokens@IsParsed(_)) => (Some(Expr.Binary(unary, op, assign)), remainingTokens)
        case _ => (None, NotParsed(tokens))
    case _ => exprOr(tokens)

def exprOr(tokens: Tokens): ParsingPair[Expr] =
  exprAnd(tokens) match
    case (Some(and), IsParsed(remainingTokens)) => exprOrPrime(remainingTokens, and)
    case _ => (None, NotParsed(tokens))

@tailrec
def exprOrPrime(tokens: Tokens, prevExpr: Expr): ParsingPair[Expr] =
  tokens match
    case (op@token.Token(OR, _)) :: tail =>
      exprAnd(tail) match
        case (Some(and), IsParsed(remainingTokens)) => exprOrPrime(remainingTokens, Expr.Binary(prevExpr, op, and))
        case _ => (None, NotParsed(tokens))
    case _ => (Some(prevExpr), IsParsed(tokens))

def exprAnd(tokens: Tokens): ParsingPair[Expr] =
  exprEq(tokens) match
    case (Some(eq), IsParsed(remainingTokens)) => exprAndPrime(remainingTokens, eq)
    case _ => (None, NotParsed(tokens))

@tailrec
def exprAndPrime(tokens: Tokens, prevExpr: Expr): ParsingPair[Expr] =
  tokens match
    case (op@token.Token(AND, _)) :: tail =>
      exprEq(tail) match
        case (Some(eq), IsParsed(remainingTokens)) => exprAndPrime(remainingTokens, Expr.Binary(prevExpr, op, eq))
        case _ => (None, NotParsed(tokens))
    case _ => (Some(prevExpr), IsParsed(tokens))

def exprEq(tokens: Tokens): ParsingPair[Expr] =
  exprRel(tokens) match
    case (Some(rel), IsParsed(remainingTokens)) => exprEqPrime(remainingTokens, rel)
    case _ => (None, NotParsed(tokens))

@tailrec
def exprEqPrime(tokens: Tokens, prevExpr: Expr): ParsingPair[Expr] =
  tokens match
    case (op@Token(EQUAL | NOTEQ, _)) :: tail =>
      exprRel(tail) match
        case (Some(rel), IsParsed(remainingTokens)) => exprEqPrime(remainingTokens, Expr.Binary(prevExpr, op, rel))
        case _ => (None, NotParsed(tokens))
    case _ => (Some(prevExpr), IsParsed(tokens))

def exprRel(tokens: Tokens): ParsingPair[Expr] =
  exprAdd(tokens) match
    case (Some(add), IsParsed(remainingTokens)) => exprRelPrime(remainingTokens, add)
    case _ => (None, NotParsed(tokens))

@tailrec
def exprRelPrime(tokens: Tokens, prevExpr: Expr): ParsingPair[Expr] =
  tokens match
    case (op@Token(LESS | LESSEQ | GREATER | GREATEREQ, _)) :: tail =>
      exprAdd(tail) match
        case (Some(add), IsParsed(remainingTokens)) => exprRelPrime(remainingTokens, Expr.Binary(prevExpr, op, add))
        case _ => (None, NotParsed(tokens))
    case _ => (Some(prevExpr), IsParsed(tokens))

def exprAdd(tokens: Tokens): ParsingPair[Expr] =
  exprMul(tokens) match
    case (Some(mul), IsParsed(remainingTokens)) => exprAddPrime(remainingTokens, mul)
    case _ => (None, NotParsed(tokens))

@tailrec
def exprAddPrime(tokens: Tokens, prevExpr: Expr): ParsingPair[Expr] =
  tokens match
    case (op@Token(ADD | SUB, _)) :: tail =>
      exprMul(tail) match
        case (Some(mul), IsParsed(remainingTokens)) => exprAddPrime(remainingTokens, Expr.Binary(prevExpr, op, mul))
        case _ => (None, NotParsed(tokens))
    case _ => (Some(prevExpr), IsParsed(tokens))

def exprMul(tokens: Tokens): ParsingPair[Expr] =
  exprCast(tokens) match
    case (Some(cast), IsParsed(remainingTokens)) => exprMulPrime(remainingTokens, cast)
    case _ => (None, NotParsed(tokens))

@tailrec
def exprMulPrime(tokens: Tokens, prevExpr: Expr): ParsingPair[Expr] =
  tokens match
    case (op@Token(MUL | DIV, _)) :: tail =>
      exprCast(tail) match
        case (Some(cast), IsParsed(remainingTokens)) => exprMulPrime(remainingTokens, Expr.Binary(prevExpr, op, cast))
        case _ => (None, NotParsed(tokens))
    case _ => (Some(prevExpr), IsParsed(tokens))


def exprCast(tokens: Tokens): ParsingPair[Expr] =
  tokens match
    case token.Token(LPAR, _) :: tail =>
      typeBase(tail) match
        case (Some(baseType), IsParsed(Token(RPAR, _) :: tail)) =>
          exprCast(tail) match
            case (Some(castExpr), remainingTokens) =>
              (Some(Expr.Cast(baseType, None, castExpr)), remainingTokens)
            case _ => (None, NotParsed(tokens))

        case (Some(baseType), IsParsed(remainingTokens)) =>
          arrayDecl(remainingTokens) match
            case (arrDecl, IsParsed(Token(RPAR, _) :: tail)) =>
              exprCast(tail) match
                case (Some(castExpr), remainingTokens) =>
                  (Some(Expr.Cast(baseType, arrDecl, castExpr)), remainingTokens)
                case _ => (None, NotParsed(tokens))
            case _ => (None, NotParsed(tokens))

        case _ => exprUnary(tokens)

    case _ => exprUnary(tokens)

def exprUnary(tokens: Tokens): ParsingPair[Expr] =
  tokens match
    case (op@Token(SUB | NOT, _)) :: tail =>
      exprUnary(tail) match
        case (Some(expUnary), remainingTokens) => (Some(Expr.Unary(op, expUnary)), remainingTokens)
        case _ => (None, NotParsed(tokens))
    case _ => exprPostfix(tokens)

def exprPostfix(tokens: Tokens): ParsingPair[Expr] =
  exprPrimary(tokens) match
    case (Some(exp), IsParsed(remainingTokens)) =>
      exprPostfixPrime(remainingTokens, exp)
    case _ => (None, NotParsed(tokens))

@tailrec
def exprPostfixPrime(tokens: Tokens, prevExpr: Expr): ParsingPair[Expr] =
  tokens match
    case token.Token(LBRACKET, _) :: tail =>
      expr(tail) match
        case (Some(exp), IsParsed((op@Token(RBRACKET, _)) :: tail)) =>
          exprPostfixPrime(tail, Expr.Binary(prevExpr, op, exp))
        case _ => (None, NotParsed(tokens))

    case (op@Token(DOT, _)) :: (id@TokenWithValue(ID, _, _)) :: tail =>
      exprPostfixPrime(tail, Expr.Binary(prevExpr, op, Expr.Variable(id)))

    case _ => (Some(prevExpr), IsParsed(tokens))

def exprPrimary(tokens: Tokens): ParsingPair[Expr] =
  @tailrec
  def exprsHelper(tokens: Tokens, exprs: List[Expr]): ParsingPair[List[Expr]] =
    tokens match
      case Token(COMMA, _) :: remainingTokens =>
        expr(remainingTokens) match
          case (Some(exp), IsParsed(remainingTokens)) => exprsHelper(remainingTokens, exprs :+ exp)
          case _ => (Some(List()), IsParsed(tokens))
      case _ => (Some(exprs), IsParsed(tokens))

  tokens match
    case (funName@Token(ID, _)) :: Token(LPAR, _) :: tail =>
      expr(tail) match
        case (Some(exp), IsParsed(remainingTokens)) =>
          exprsHelper(remainingTokens, List(exp)) match
            case (Some(exprs), IsParsed(Token(RPAR, _) :: remainingTokens)) => (Option(Expr.FunctionCall(funName, exprs*)), IsParsed(remainingTokens))
            case _ => (None, NotParsed(tokens))
        case _ => (None, NotParsed(tokens))

    case (varName@TokenWithValue(ID, _, _)) :: tail => (Some(Expr.Variable(varName)), IsParsed(tail))

    case (t@TokenWithValue(INT | DOUBLE | CHAR | STRING, _, _)) :: tail => (Some(Expr.Literal(t)), IsParsed(tail))

    case Token(LPAR, _) :: tail =>
      expr(tail) match
        case (exp, IsParsed(Token(RPAR, _) :: tail)) =>
          (exp, IsParsed(tail))
        case _ => (None, NotParsed(tokens))

    case _ => (None, NotParsed(tokens))

def stmCompound(tokens: Tokens): ParsingPair[Statement.StmCompound] =
  @tailrec
  def stmCompoundHelper(crtTokens: Tokens, definitions: List[Statement]): ParsingPair[List[Statement]] =
    varDef(crtTokens) match
      case (Some(definition), IsParsed(remainingTokens)) => stmCompoundHelper(remainingTokens, definitions :+ definition)
      case _ =>
        stm(crtTokens) match
          case (Some(statement), IsParsed(remainingTokens)) => stmCompoundHelper(remainingTokens, definitions :+ statement)
          case _ => (Some(definitions), IsParsed(crtTokens))

  tokens match
    case Token(LACC, _) :: remainingTokens =>
      remainingTokens match
        case Token(RACC, _) :: tail => (Option(Statement.StmCompound()), IsParsed(tail))
        case _ =>
          stmCompoundHelper(remainingTokens, List()) match
            case (Some(stms), IsParsed(remainingTokens)) =>
              remainingTokens match
                case Token(RACC, _) :: tail => (Option(Statement.StmCompound(stms*)), IsParsed(tail))
                case _ => (None, NotParsed(tokens))
            case _ => (None, NotParsed(tokens))
    case _ => (None, NotParsed(tokens))


def varDef_*(tokens: Tokens): ParsingPair[List[Statement.VarDef]] =
  @tailrec
  def varDefsHelper(crtTokens: Tokens, definitions: List[Statement.VarDef]): ParsingPair[List[Statement.VarDef]] =
    varDef(crtTokens) match
      case (Some(definition), IsParsed(remainingTokens)) => varDefsHelper(remainingTokens, definitions :+ definition)
      case _ => (Option(definitions), IsParsed(crtTokens))

  varDefsHelper(tokens, List())

def pprint(ast: AstNode, depth: Int = 0): Unit =
  val ident = "\t" * depth
  ast match
    case UnitRule(definitions*) =>
      println("UnitRule:")
      definitions.foreach(d => pprint(d, depth + 1))

    case Aux.TypeBase(baseType, structId) =>
      val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t).trim}").getOrElse("")
      print(s"${ident}$typeBaseStr")

    case Statement.VarDef(baseType, id, arrayDecl) =>
      println(s"${ident}VarDef:")
      pprint(baseType, depth + 1)
      print(s" ${stringify(id)}")
      print(arrayDecl.map(d => s"[${stringify(d.arrSize).trim}]\n").getOrElse("\n"))

    case Statement.StructDef(id, varDefs*) =>
      println(s"${ident}StructDef:")
      ident + stringify(id).trim + varDefs.foreach(d => pprint(d, depth + 1))

    case Statement.FnDef(Aux.TypeBase(baseType, structId), id, stm, params*) =>
      val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t).trim}").getOrElse("")
      println(s"${ident}FnDef:")
      println(s"$ident\t$typeBaseStr ${stringify(id).trim}")
      params.foreach(p => pprint(p, depth + 1))
      pprint(stm, depth + 1)

    case Aux.FnParam(Aux.TypeBase(baseType, structId), id, arrayDecl) =>
      val typeBaseStr = stringify(baseType) + structId.map(t => s" ${stringify(t).trim}").getOrElse("")
      println(s"${ident}FnParam:\n$ident\t" + typeBaseStr + " " + stringify(id) +
        arrayDecl.map(d => s"[${stringify(d.arrSize).trim}]").getOrElse(""))

    case Statement.StmCompound(stms*) =>
      println(s"${ident}StmCompound:")
      stms.foreach(s => pprint(s, depth + 1))

    case Expr.FunctionCall(funName, exprs*) =>
      println(s"${ident}FunctionCall:")
      println(s"$ident\tname: ${stringify(funName)}")
      println(s"$ident\tParams:")
      exprs.foreach(e => pprint(e, depth + 2))

    case Expr.Literal(TokenWithValue(code, _, value)) =>
      println(s"${ident}Literal:")
      code match
        case INT => println(s"$ident\tvalue: $value")
        case STRING => println(s"$ident\tvalue: \"$value\"")
        case CHAR => println(s"$ident\tvalue: \'$value\'")
        case _ =>

    case Expr.Binary(left, op, right) =>
      println(s"${ident}Binary:")
      pprint(left, depth + 1)
      println(s"$ident\t${op.tokenCode}")
      pprint(right, depth + 1)

    case Expr.Unary(op, expr) =>
      println(s"${ident}Unary:")
      println(s"$ident\t${op.tokenCode}")
      pprint(expr, depth + 1)

    case Expr.Variable(t) =>
      println(s"${ident}Variable:")
      println(s"$ident\tvalue: ${stringify(t)}")

    case Expr.Cast(baseType, arrayDecl, castedExpr) =>
      println(s"${ident}Cast:")
      print(s"${ident}\tcast to: ")
      pprint(baseType)
      print(arrayDecl.map(d => s"[${stringify(d.arrSize).trim}]\n").getOrElse("\n"))
      pprint(castedExpr, depth + 1)

    case Statement.If(cond, thenBranch, elseBranch) =>
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

    case Statement.While(cond, body) =>
      println(s"${ident}While:")
      println(s"$ident\tcond:")
      pprint(cond, depth + 2)
      println(s"$ident\tbody:")
      pprint(body, depth + 2)

    case Statement.Return(expr) =>
      println(s"${ident}Return:")
      expr.foreach(r => pprint(r, depth + 1))

    case Statement.ExpressionStatement(expr) =>
      println(s"${ident}ExpressionStatement:")
      expr.foreach(e => pprint(e, depth + 1))

    case _ =>

class Test:
  val a = 0

case object Test extends App:
  val tokens = Lexer(new File("testlex.c")).tokenizeFile
  pprint(unit(tokens)._1.get)
