package parser.ast

import parser.ast.DefinitionUtils.{arraySize, typeBase}
import parser.exceptions.SyntaxError
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import token.TokenCode.*
import token.{Token, TokenWithValue}

import scala.annotation.tailrec

sealed trait ExpressionNode extends AstNode

object ExpressionNode:

  final case class BinaryExprNode
  (
    left: ExpressionNode,
    operator: Token,
    right: ExpressionNode
  ) extends ExpressionNode

  final case class UnaryExprNode
  (
    operator: Token,
    right: ExpressionNode
  ) extends ExpressionNode

  final case class FunctionCallExprNode
  (
    funName: Token,
    expressions: ExpressionNode *
  ) extends ExpressionNode

  final case class LiteralExprNode[T]
  (
    literal: TokenWithValue[T]
  ) extends ExpressionNode

  final case class VariableExprNode[T]
  (
    variable: TokenWithValue[T]
  ) extends  ExpressionNode

  final case class CastExprNode
  (
    typeBase: DefinitionUtils.TypeBaseNode,
    arraySize: Option[DefinitionUtils.ArraySizeNode] = None,
    castedExpr: ExpressionNode
  ) extends ExpressionNode

  def expr(tokens: Tokens): ParsingPair[ExpressionNode] =
    exprAssign(tokens)

  def exprAssign(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprUnary ASSIGN exprAssign | exprOr
    exprUnary(tokens) match
      case (Some(unary), IsParsed((op@Token(ASSIGN, _)) :: tail)) =>
        exprAssign(tail) match
          case (Some(assign), remainingTokens@IsParsed(_)) =>
            (Some(BinaryExprNode(unary, op, assign)), remainingTokens)

          // should have expression after =
          case (None, NotParsed(t :: _)) => throw SyntaxError("the right hand side of an assignment", t, tokens)

          case _ => (None, NotParsed(tokens))
      case _ => exprOr(tokens)

  def exprOr(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprAnd exprOrPrime
    exprAnd(tokens) match
      case (Some(and), IsParsed(remainingTokens)) => exprOrPrime(remainingTokens, and, tokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprOrPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // OR exprAnd exprOrPrime | eps
    tokens match
      case (op@token.Token(OR, _)) :: tail =>
        exprAnd(tail) match
          case (Some(and), IsParsed(remainingTokens)) =>
            exprOrPrime(remainingTokens, BinaryExprNode(prevExpr, op, and), contextTokens)

          // should have expression after ||
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an or expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprAnd(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprEq exprAndPrime
    exprEq(tokens) match
      case (Some(eq), IsParsed(remainingTokens)) => exprAndPrime(remainingTokens, eq, tokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprAndPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // AND exprEq exprAndPrime | eps
    tokens match
      case (op@token.Token(AND, _)) :: tail =>
        exprEq(tail) match
          case (Some(eq), IsParsed(remainingTokens)) =>
            exprAndPrime(remainingTokens, BinaryExprNode(prevExpr, op, eq), tokens)

          // should have expression after &&
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an and expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprEq(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprRel exprEqPrime
    exprRel(tokens) match
      case (Some(rel), IsParsed(remainingTokens)) => exprEqPrime(remainingTokens, rel, tokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprEqPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // (EQUAL | NOTEQ) exprRel exprEqPrime | eps
    tokens match
      case (op@Token(EQUAL | NOTEQ, _)) :: tail =>
        exprRel(tail) match
          case (Some(rel), IsParsed(remainingTokens)) =>
            exprEqPrime(remainingTokens, BinaryExprNode(prevExpr, op, rel), contextTokens)

          // should have expression after equality operators
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an equality expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprRel(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprAdd exprRelPrime
    exprAdd(tokens) match
      case (Some(add), IsParsed(remainingTokens)) => exprRelPrime(remainingTokens, add, tokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprRelPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrime | eps
    tokens match
      case (op@Token(LESS | LESSEQ | GREATER | GREATEREQ, _)) :: tail =>
        exprAdd(tail) match
          case (Some(add), IsParsed(remainingTokens)) =>
            exprRelPrime(remainingTokens, BinaryExprNode(prevExpr, op, add), contextTokens)

          // should have expression after comparison operator
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of a comparison expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprAdd(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprMul exprAddPrime
    exprMul(tokens) match
      case (Some(mul), IsParsed(remainingTokens)) => exprAddPrime(remainingTokens, mul, tokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprAddPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // (ADD | SUB) exprMul exprAddPrime | eps
    tokens match
      case (op@Token(ADD | SUB, _)) :: tail =>
        exprMul(tail) match
          case (Some(mul), IsParsed(remainingTokens)) =>
            exprAddPrime(remainingTokens, BinaryExprNode(prevExpr, op, mul), contextTokens)

          // should have expression after operator
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an arithmetic expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprMul(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprCast exprMulPrime
    exprCast(tokens) match
      case (Some(cast), IsParsed(remainingTokens)) => exprMulPrime(remainingTokens, cast, tokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprMulPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // (MUL | DIV) exprCast exprMulPrime | eps
    tokens match
      case (op@Token(MUL | DIV, _)) :: tail =>
        exprCast(tail) match
          case (Some(cast), IsParsed(remainingTokens)) =>
            exprMulPrime(remainingTokens, BinaryExprNode(prevExpr, op, cast), contextTokens)

          // should have expression after operator
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an arithmetic expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))


  def exprCast(tokens: Tokens): ParsingPair[ExpressionNode] =
    // LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
    tokens match
      case token.Token(LPAR, _) :: tail =>
        typeBase(tail) match
          case (Some(baseType), IsParsed(remainingTokens)) =>
            arraySize(remainingTokens, tokens) match
              case (arrDecl, remainingTokens) =>
                remainingTokens.get match
                  case Token(RPAR, _) :: tail =>
                    exprCast(tail) match
                      case (Some(castExpr), remainingTokens) =>
                        (Some(CastExprNode(baseType, arrDecl, castExpr)), remainingTokens)
                      case _ => (None, NotParsed(tokens))

                  // should have the matching )
                  case t :: _ => throw SyntaxError(RPAR, t, tokens)

                  case _ => (None, NotParsed(tokens))
          case _ => exprUnary(tokens)
      case _ => exprUnary(tokens)

  def exprUnary(tokens: Tokens): ParsingPair[ExpressionNode] =
    tokens match
      case (op@Token(SUB | NOT, _)) :: tail =>
        exprUnary(tail) match
          case (Some(expUnary), remainingTokens) => (Some(UnaryExprNode(op, expUnary)), remainingTokens)

          // should have expression after operator
          case (None, NotParsed(t :: _)) => throw SyntaxError("an unary expression", t, tokens)

          case _ => (None, NotParsed(tokens))
      case _ => exprPostfix(tokens)

  def exprPostfix(tokens: Tokens): ParsingPair[ExpressionNode] =
    // exprPrimary exprPostfixPrime
    exprPrimary(tokens) match
      case (Some(exp), IsParsed(remainingTokens)) =>
        exprPostfixPrime(remainingTokens, exp, tokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprPostfixPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // LBRACKET expr RBRACKET exprPostfixPrime | DOT ID exprPostfixPrime | eps
    tokens match
      case token.Token(LBRACKET, _) :: tail =>
        expr(tail) match
          case (Some(exp), IsParsed((op@Token(RBRACKET, _)) :: tail)) =>
            exprPostfixPrime(tail, BinaryExprNode(prevExpr, op, exp), contextTokens)

          // should have the matching ]
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(RBRACKET, t, contextTokens)

          // should have expression between []
          case (None, NotParsed(t :: _)) => throw SyntaxError("an expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))

      case (op@Token(DOT, _)) :: (id@TokenWithValue(ID, _, _)) :: tail =>
        exprPostfixPrime(tail, BinaryExprNode(prevExpr, op, VariableExprNode(id)), contextTokens)

      // should have id after .
      case Token(DOT, _) :: t :: _ => throw SyntaxError(ID, t, contextTokens)

      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprPrimary(tokens: Tokens): ParsingPair[ExpressionNode] =
    // ID(LPAR(expr(COMMA expr) *) ? RPAR) ? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
    @tailrec
    def exprHelper(tokens: Tokens, expressions: List[ExpressionNode]): ParsingPair[List[ExpressionNode]] =
      tokens match
        case Token(COMMA, _) :: remainingTokens =>
          expr(remainingTokens) match
            case (Some(e), IsParsed(remainingTokens)) => exprHelper(remainingTokens, expressions :+ e)
            case _ => (Some(List()), IsParsed(tokens))

        // should have , between function arguments
        case t :: _ if t.tokenCode != RPAR && t.tokenCode != LACC => throw SyntaxError(COMMA, t, tokens)

        case _ => (Some(expressions), IsParsed(tokens))

    tokens match
      // ID(LPAR(expr(COMMA expr) *) ? RPAR)
      case (funName@Token(ID, _)) :: Token(LPAR, _) :: tail =>
        expr(tail) match
          case (Some(exp), IsParsed(remainingTokens)) =>
            exprHelper(remainingTokens, List(exp)) match
              case (Some(exprs), IsParsed(Token(RPAR, _) :: remainingTokens)) =>
                (Option(FunctionCallExprNode(funName, exprs *)), IsParsed(remainingTokens))

              // should have ) at the end of function call
              case (Some(_), IsParsed(t :: _)) => throw SyntaxError(RPAR, t, tokens)

              case _ => (None, NotParsed(tokens))

          // function called without params
          case (None, NotParsed(Token(RPAR, _) :: remainingTokens)) => (Option(FunctionCallExprNode(funName)), IsParsed(remainingTokens))

          case _ => (None, NotParsed(tokens))

      // ID
      case (varName@TokenWithValue(ID, _, _)) :: tail => (Some(VariableExprNode(varName)), IsParsed(tail))

      // INT | DOUBLE | CHAR | STRING
      case (t@TokenWithValue(INT | DOUBLE | CHAR | STRING, _, _)) :: tail => (Some(LiteralExprNode(t)), IsParsed(tail))

      // LPAR expr RPAR
      case Token(LPAR, _) :: tail =>
        expr(tail) match
          case (exp, IsParsed(Token(RPAR, _) :: tail)) =>(exp, IsParsed(tail))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(RPAR, t, tokens)

          case _ => (None, NotParsed(tokens))

      case _ => (None, NotParsed(tokens))
