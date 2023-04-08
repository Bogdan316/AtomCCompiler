package parser

import lexer.Lexer
import parser.ast.*
import parser.ast.AstRoot.*
import parser.ast.Aux.*
import parser.ast.Definition.*
import parser.ast.Expression.*
import parser.ast.Statement.*
import parser.parsed.*
import parser.utils.PrettyPrint
import token.Token.stringify
import token.TokenCode.*
import token.{Token, TokenWithValue}
import utils.*

import java.io.File
import scala.annotation.{tailrec, targetName}


case class Parser(originalTokens: Tokens):
  def unit(tokens: Tokens): ParsingPair[UnitRule] =
    @tailrec
    //  this needs to return the remaining tokens or just check for end
    def unitHelper(tokens: Tokens, defs: List[Definition]): List[Definition] =
      fnDef(tokens) match
        case (Some(varDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ varDefinition)
        case _ =>
          varDef(tokens) match
            case (Some(structDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ structDefinition)
            case _ => structDef(tokens) match
              case (Some(functionDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ functionDefinition)
              case (None, NotParsed(Token(END, _) :: Nil)) => defs
              case _ => throw ExpectedButFoundError(END, tokens.head, None)

    (Option(UnitRule(unitHelper(tokens, List()) *)), IsParsed(tokens))

  def structDef(tokens: Tokens): ParsingPair[StructDef] =
    @tailrec
    def varDefHelper(crtTokens: Tokens, definitions: List[VariableDef]): ParsingPair[List[VariableDef]] =
      varDef(crtTokens) match
        case (Some(definition), IsParsed(remainingTokens)) => varDefHelper(remainingTokens, definitions :+ definition)
        case _ => (Option(definitions), IsParsed(crtTokens))

    tokens match
      case Token(STRUCT, _) :: (structId@Token(ID, _)) :: Token(LACC, _) :: remainingTokens =>
        varDefHelper(remainingTokens, List()) match
          case (Some(definitions), IsParsed(Token(RACC, _) :: Token(SEMICOLON, _) :: remainingTokens)) =>
            (Option(StructDef(structId, definitions *)), IsParsed(remainingTokens))

          case (Some(_), IsParsed(Token(RACC, _) :: t :: _)) =>
            throw ExpectedButFoundError(SEMICOLON, t, Option(tokens.span(_ ne t)._1))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(RACC, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))

      case ts@Token(STRUCT, _) :: Token(ID, _) :: t :: _ =>
        throw ExpectedButFoundError(LACC, t, Option(ts.take(2)))

      case ts@Token(STRUCT, _) :: t :: _ =>
        throw ExpectedButFoundError(ID, t, Option(ts.take(1)))

      case _ => (None, NotParsed(tokens))

  def varDef(tokens: Tokens): ParsingPair[VariableDef] =
    typeBase(tokens) match
      case (Some(varType), IsParsed((varId@Token(ID, _)) :: Token(SEMICOLON, _) :: remainingTokens)) =>
        (Option(VariableDef(varType, varId)), IsParsed(remainingTokens))

      // should have ; at the end of definition if function body is not next
      case (Some(_), IsParsed(Token(ID, _) :: t :: _)) if t.tokenCode != LBRACKET =>
        throw ExpectedButFoundError(SEMICOLON, t, Option(tokens.span(_ ne t)._1))

      case (Some(varType), IsParsed((varId@Token(ID, _)) :: remainingTokens)) =>
        arrayDecl(remainingTokens, tokens) match
          case (arraySize, IsParsed(Token(SEMICOLON, _) :: remainingTokens)) =>
            (Some(VariableDef(varType, varId, arraySize)), IsParsed(remainingTokens))

          // should have ; at the end of definition if function body is not next
          case (Some(_), IsParsed(t :: _)) if t.tokenCode != LBRACKET =>
            throw ExpectedButFoundError(SEMICOLON, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))

      // should have id after type
      case (Some(_), IsParsed(t :: _)) if t.tokenCode != LACC =>
        throw ExpectedButFoundError(ID, t, Option(tokens.span(_ ne t)._1))

      case _ => (None, NotParsed(tokens))

  def typeBase(tokens: Tokens): ParsingPair[TypeBase] =
    tokens match
      case (definedType@Token(TYPE_INT | TYPE_DOUBLE | TYPE_CHAR, _)) :: tail =>
        (Option(TypeBase(definedType)), IsParsed(tail))

      case (structType@Token(STRUCT, _)) :: (structId@Token(ID, _)) :: tail =>
        (Option(TypeBase(structType, Option(structId))), IsParsed(tail))

      // struct keyword should be followed by an id
      case Token(STRUCT, _) :: t :: _ =>
        throw ExpectedButFoundError(ID, t, Option(tokens.span(_ ne t)._1))

      case _ => (None, NotParsed(tokens))

  def arrayDecl(tokens: Tokens, contextTokens: Tokens): ParsingPair[ArraySize] =
    tokens match
      case Token(LBRACKET, _) :: Token(RBRACKET, _) :: tail => (Option(ArraySize()), IsParsed(tail))

      case Token(LBRACKET, _) :: (arrSize@Token(INT, _)) :: Token(RBRACKET, _) :: tail =>
        (Option(ArraySize(arrSize)), IsParsed(tail))

      case Token(LBRACKET, _) :: t :: Token(RBRACKET, _) :: _ =>
        throw ExpectedButFoundError(INT, t, Option(contextTokens.span(_ ne t)._1))

      case Token(LBRACKET, _) :: t :: _ =>
        throw ExpectedButFoundError(RBRACKET, t, Option(contextTokens.span(_ ne t)._1))

      case _ => (None, NotParsed(tokens))

  def fnParam(tokens: Tokens, contextTokens: Tokens): ParsingPair[FunctionParam] =
    typeBase(tokens) match
      case (Some(paramType), IsParsed((paramId@Token(ID, _)) :: remainingTokens)) =>
        arrayDecl(remainingTokens, contextTokens) match
          case (arrayDecl, remainingTokens@IsParsed(_)) =>
            (Option(FunctionParam(paramType, paramId, arrayDecl)), remainingTokens)

          case _ => (Option(FunctionParam(paramType, paramId)), IsParsed(remainingTokens))

      case (Some(_), IsParsed(t :: _)) =>
        throw ExpectedButFoundError(ID, t, Option(contextTokens.span(_ ne t)._1))

      case _ => (None, NotParsed(tokens))

  def fnDef(tokens: Tokens): ParsingPair[FunctionDef] =
    val (returnType, fnName, remainingTokens) =
      tokens match
        case (retType@Token(VOID, _)) :: (fnName@Token(ID, _)) :: remainingTokens =>
          (Some(TypeBase(retType)), Some(fnName), IsParsed(remainingTokens))

        case Token(VOID, _) :: t :: _ =>
          throw ExpectedButFoundError(ID, t, Option(tokens.span(_ ne t)._1))

        case _ =>
          typeBase(tokens) match
            case (Some(baseType), IsParsed((fnName@Token(ID, _)) :: remainingTokens)) =>
              (Some(baseType), Some(fnName), IsParsed(remainingTokens))

            case (Some(_), IsParsed(t :: _)) if t.tokenCode != LACC =>
              throw ExpectedButFoundError(ID, t, Option(tokens.span(_ ne t)._1))

            case _ => (None, None, NotParsed(tokens))

    remainingTokens match
      case IsParsed(Token(LPAR, _) :: remainingTokens) =>
        fnParams(remainingTokens, tokens) match
          case (Some(params), IsParsed(Token(RPAR, _) :: remainingTokens)) =>

            stmCompound(remainingTokens) match
              case (Some(statement), remainingTokens) =>
                (Option(FunctionDef(returnType.get, fnName.get, statement, params *)), remainingTokens)

              case _ => (None, NotParsed(tokens))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(RPAR, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))

      case IsParsed(t :: Token(LPAR, _) :: _) =>
        throw ExpectedButFoundError(ID, t, Option(tokens.span(_ ne t)._1))

      case _ => (None, NotParsed(tokens))

  def fnParams(tokens: Tokens, contextTokens: Tokens): ParsingPair[List[FunctionParam]] =
    @tailrec
    def fnParamsHelper(crtTokens: Tokens, params: List[FunctionParam]): ParsingPair[List[FunctionParam]] =
      crtTokens match
        case Token(COMMA, _) :: remainingTokens =>
          fnParam(remainingTokens, contextTokens) match
            case (Some(param), IsParsed(remainingTokens)) => fnParamsHelper(remainingTokens, params :+ param)

            case _ => (Some(List()), IsParsed(crtTokens))

        case t :: _  if t.tokenCode != RPAR && t.tokenCode != LACC =>
          throw ExpectedButFoundError(COMMA, t, Option(contextTokens.span(_ ne t)._1))

        case _ => (Some(params), IsParsed(crtTokens))


    fnParam(tokens, contextTokens) match
      case (Some(param), IsParsed(remainingTokens)) =>
        fnParamsHelper(remainingTokens, List()) match
          case (Some(params), remainingTokens@IsParsed(_)) => (Option(param +: params), remainingTokens)

          case _ => (Some(List(param)), IsParsed(remainingTokens))

      case _ => (Some(List()), IsParsed(tokens))

  def stm(tokens: Tokens): ParsingPair[Statement] =
    tokens match
      case Token(IF, _) :: Token(LPAR, _) :: tail =>
        expr(tail) match
          case (Some(ifExp), IsParsed(Token(RPAR, _) :: tail)) =>

            stm(tail) match
              case (Some(thenBranch), IsParsed(Token(ELSE, _) :: tail)) =>

                stm(tail) match
                  case (elseBranch@Some(_), remainingTokens@IsParsed(_)) =>
                    (Some(IfStm(ifExp, thenBranch, elseBranch)), remainingTokens)

                  case _ => (None, NotParsed(tokens))

              case (Some(thenBranch), remainingTokens@IsParsed(_)) =>
                (Some(IfStm(ifExp, thenBranch)), remainingTokens)

              case _ => (None, NotParsed(tokens))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(RPAR, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))

      case Token(IF, _) :: t :: _ =>
        throw ExpectedButFoundError(LPAR, t, Option(tokens.span(_ ne t)._1))

      case Token(WHILE, _) :: Token(LPAR, _) :: tail =>
        expr(tail) match
          case (Some(cond), IsParsed(Token(RPAR, _) :: tail)) =>

            stm(tail) match
              case (Some(body), remainingTokens@IsParsed(_)) =>
                (Some(WhileStm(cond, body)), remainingTokens)

              case _ => (None, NotParsed(tokens))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(RPAR, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))

      case Token(WHILE, _) :: t :: _ =>
        throw ExpectedButFoundError(LPAR, t, Option(tokens.span(_ ne t)._1))

      case Token(RETURN, _) :: Token(SEMICOLON, _) :: tail => (Some(ReturnStm()), IsParsed(tail))

      case Token(RETURN, _) :: tail =>
        expr(tail) match
          case (returnExpr@Some(_), IsParsed(Token(SEMICOLON, _) :: tail)) =>
            (Some(ReturnStm(returnExpr)), IsParsed(tail))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(SEMICOLON, t, Option(tokens.span(_ ne t)._1))

          case (_, NotParsed(t :: _)) =>
            throw ExpectedButFoundError(SEMICOLON, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))

      case Token(SEMICOLON, _) :: tail => (Some(ExpressionStm()), IsParsed(tail))

      case Token(LACC, _) :: _ => stmCompound(tokens)

      case t =>
        expr(t) match
          case (expr@Some(_), IsParsed(Token(SEMICOLON, _) :: tail)) =>
            (Some(ExpressionStm(expr)), IsParsed(tail))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(SEMICOLON, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))


  def expr(tokens: Tokens): ParsingPair[Expression] =
    exprAssign(tokens)

  def exprAssign(tokens: Tokens): ParsingPair[Expression] =
    exprOr(tokens) match
      case orExpr@(Some(_), IsParsed(_)) => orExpr
      case _ =>
        exprUnary(tokens) match
          case (Some(unary), IsParsed((op@Token(ASSIGN, _)) :: tail)) =>
            exprAssign(tail) match
              case (Some(assign), remainingTokens@IsParsed(_)) =>
                (Some(BinaryExpr(unary, op, assign)), remainingTokens)

              case _ => (None, NotParsed(tokens))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(ASSIGN, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))


  def exprOr(tokens: Tokens): ParsingPair[Expression] =
    exprAnd(tokens) match
      case (Some(and), IsParsed(remainingTokens)) => exprOrPrime(remainingTokens, and)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprOrPrime(tokens: Tokens, prevExpr: Expression): ParsingPair[Expression] =
    tokens match
      case (op@token.Token(OR, _)) :: tail =>
        exprAnd(tail) match
          case (Some(and), IsParsed(remainingTokens)) => exprOrPrime(remainingTokens, BinaryExpr(prevExpr, op, and))
          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprAnd(tokens: Tokens): ParsingPair[Expression] =
    exprEq(tokens) match
      case (Some(eq), IsParsed(remainingTokens)) => exprAndPrime(remainingTokens, eq)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprAndPrime(tokens: Tokens, prevExpr: Expression): ParsingPair[Expression] =
    tokens match
      case (op@token.Token(AND, _)) :: tail =>
        exprEq(tail) match
          case (Some(eq), IsParsed(remainingTokens)) => exprAndPrime(remainingTokens, BinaryExpr(prevExpr, op, eq))
          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprEq(tokens: Tokens): ParsingPair[Expression] =
    exprRel(tokens) match
      case (Some(rel), IsParsed(remainingTokens)) => exprEqPrime(remainingTokens, rel)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprEqPrime(tokens: Tokens, prevExpr: Expression): ParsingPair[Expression] =
    tokens match
      case (op@Token(EQUAL | NOTEQ, _)) :: tail =>
        exprRel(tail) match
          case (Some(rel), IsParsed(remainingTokens)) => exprEqPrime(remainingTokens, BinaryExpr(prevExpr, op, rel))
          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprRel(tokens: Tokens): ParsingPair[Expression] =
    exprAdd(tokens) match
      case (Some(add), IsParsed(remainingTokens)) => exprRelPrime(remainingTokens, add)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprRelPrime(tokens: Tokens, prevExpr: Expression): ParsingPair[Expression] =
    tokens match
      case (op@Token(LESS | LESSEQ | GREATER | GREATEREQ, _)) :: tail =>
        exprAdd(tail) match
          case (Some(add), IsParsed(remainingTokens)) => exprRelPrime(remainingTokens, BinaryExpr(prevExpr, op, add))
          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprAdd(tokens: Tokens): ParsingPair[Expression] =
    exprMul(tokens) match
      case (Some(mul), IsParsed(remainingTokens)) => exprAddPrime(remainingTokens, mul)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprAddPrime(tokens: Tokens, prevExpr: Expression): ParsingPair[Expression] =
    tokens match
      case (op@Token(ADD | SUB, _)) :: tail =>
        exprMul(tail) match
          case (Some(mul), IsParsed(remainingTokens)) => exprAddPrime(remainingTokens, BinaryExpr(prevExpr, op, mul))
          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprMul(tokens: Tokens): ParsingPair[Expression] =
    exprCast(tokens) match
      case (Some(cast), IsParsed(remainingTokens)) => exprMulPrime(remainingTokens, cast)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprMulPrime(tokens: Tokens, prevExpr: Expression): ParsingPair[Expression] =
    tokens match
      case (op@Token(MUL | DIV, _)) :: tail =>
        exprCast(tail) match
          case (Some(cast), IsParsed(remainingTokens)) => exprMulPrime(remainingTokens, BinaryExpr(prevExpr, op, cast))
          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))


  def exprCast(tokens: Tokens): ParsingPair[Expression] =
    tokens match
      case token.Token(LPAR, _) :: tail =>
        typeBase(tail) match
          case (Some(baseType), IsParsed(Token(RPAR, _) :: tail)) =>
            exprCast(tail) match
              case (Some(castExpr), remainingTokens) =>
                (Some(CastExpr(baseType, None, castExpr)), remainingTokens)
              case _ => (None, NotParsed(tokens))

          case (Some(baseType), IsParsed(remainingTokens)) =>
            arrayDecl(remainingTokens, tokens) match
              case (arrDecl, IsParsed(Token(RPAR, _) :: tail)) =>
                exprCast(tail) match
                  case (Some(castExpr), remainingTokens) =>
                    (Some(CastExpr(baseType, arrDecl, castExpr)), remainingTokens)
                  case _ => (None, NotParsed(tokens))
              case _ => (None, NotParsed(tokens))

          case _ => exprUnary(tokens)

      case _ => exprUnary(tokens)

  def exprUnary(tokens: Tokens): ParsingPair[Expression] =
    tokens match
      case (op@Token(SUB | NOT, _)) :: tail =>
        exprUnary(tail) match
          case (Some(expUnary), remainingTokens) => (Some(UnaryExpr(op, expUnary)), remainingTokens)
          case _ => (None, NotParsed(tokens))
      case _ => exprPostfix(tokens)

  def exprPostfix(tokens: Tokens): ParsingPair[Expression] =
    exprPrimary(tokens) match
      case (Some(exp), IsParsed(remainingTokens)) =>
        exprPostfixPrime(remainingTokens, exp)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprPostfixPrime(tokens: Tokens, prevExpr: Expression): ParsingPair[Expression] =
    tokens match
      case token.Token(LBRACKET, _) :: tail =>
        expr(tail) match
          case (Some(exp), IsParsed((op@Token(RBRACKET, _)) :: tail)) =>
            exprPostfixPrime(tail, BinaryExpr(prevExpr, op, exp))
          case _ => (None, NotParsed(tokens))

      case (op@Token(DOT, _)) :: (id@TokenWithValue(ID, _, _)) :: tail =>
        exprPostfixPrime(tail, BinaryExpr(prevExpr, op, VariableExpr(id)))

      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprPrimary(tokens: Tokens): ParsingPair[Expression] =
    @tailrec
    def exprsHelper(tokens: Tokens, exprs: List[Expression]): ParsingPair[List[Expression]] =
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
              case (Some(exprs), IsParsed(Token(RPAR, _) :: remainingTokens)) => (Option(FunctionCallExpr(funName, exprs *)), IsParsed(remainingTokens))
              case _ => (None, NotParsed(tokens))
          case _ => (None, NotParsed(tokens))

      case (varName@TokenWithValue(ID, _, _)) :: tail => (Some(VariableExpr(varName)), IsParsed(tail))

      case (t@TokenWithValue(INT | DOUBLE | CHAR | STRING, _, _)) :: tail => (Some(LiteralExpr(t)), IsParsed(tail))

      case Token(LPAR, _) :: tail =>
        expr(tail) match
          case (exp, IsParsed(Token(RPAR, _) :: tail)) =>
            (exp, IsParsed(tail))
          case _ => (None, NotParsed(tokens))

      case _ => (None, NotParsed(tokens))

  def stmCompound(tokens: Tokens): ParsingPair[CompoundStm] =
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
          case Token(RACC, _) :: tail => (Option(CompoundStm()), IsParsed(tail))
          case _ =>
            stmCompoundHelper(remainingTokens, List()) match
              case (Some(stms), IsParsed(remainingTokens)) =>
                remainingTokens match
                  case Token(RACC, _) :: tail => (Option(CompoundStm(stms *)), IsParsed(tail))
                  case _ => (None, NotParsed(tokens))
              case _ => (None, NotParsed(tokens))
      case _ => (None, NotParsed(tokens))

  def parse: AstNode =
    unit(originalTokens)._1.get

case object Parser extends App :
  val a = Lexer(new File("testlex.c")).tokenizeFile
  PrettyPrint.pprint(Parser(a).parse)
