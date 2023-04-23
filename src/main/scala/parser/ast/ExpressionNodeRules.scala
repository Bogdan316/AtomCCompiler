package parser.ast

import parser.ast.AstNode.DefinitionNode.*
import parser.ast.AstNode.DefinitionUtils.*
import parser.ast.AstNode.ExpressionNode
import parser.ast.AstNode.ExpressionNode.*
import parser.ast.DefinitionUtilsRules.*
import parser.exceptions.SyntaxError
import parser.parsed.{ParsingPair, Tokens}
import parser.parsed.Parsed.*
import token.Token.DelimiterToken.*
import token.Token.LiteralToken
import token.Token.OperatorToken.*
import token.Token.{IdentifierToken, LiteralToken}
import token.Token

import scala.annotation.tailrec

object ExpressionNodeRules:

  def expr(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    exprAssign(tokens, contextTokens)

  def exprAssign(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprUnary ASSIGN exprAssign | exprOr
    exprUnary(tokens, contextTokens) match
      case (Some(unary), IsParsed(AssignToken(_) :: tail)) =>
        exprAssign(tail, contextTokens) match
          case (Some(assign), remainingTokens@IsParsed(_)) =>
            (Some(AssignmentExprNode(unary, assign)), remainingTokens)

          // should have expression after =
          case (None, NotParsed(t :: _)) => throw SyntaxError("the right hand side of an assignment", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => exprOr(tokens, contextTokens)

  def exprOr(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprAnd exprOrPrime
    exprAnd(tokens, contextTokens) match
      case (Some(and), IsParsed(remainingTokens)) => exprOrPrime(remainingTokens, and, contextTokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprOrPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // OR exprAnd exprOrPrime | eps
    tokens match
      case (op@OrToken(_)) :: tail =>
        exprAnd(tail, contextTokens) match
          case (Some(and), IsParsed(remainingTokens)) =>
            exprOrPrime(remainingTokens, BinaryExprNode(prevExpr, op, and), contextTokens)

          // should have expression after ||
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an or expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprAnd(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprEq exprAndPrime
    exprEq(tokens, contextTokens) match
      case (Some(eqToken), IsParsed(remainingTokens)) => exprAndPrime(remainingTokens, eqToken, contextTokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprAndPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // AND exprEq exprAndPrime | eps
    tokens match
      case (op@AndToken(_)) :: tail =>
        exprEq(tail, contextTokens) match
          case (Some(eq), IsParsed(remainingTokens)) =>
            exprAndPrime(remainingTokens, BinaryExprNode(prevExpr, op, eq), tokens)

          // should have expression after &&
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an and expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprEq(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprRel exprEqPrime
    exprRel(tokens, contextTokens) match
      case (Some(rel), IsParsed(remainingTokens)) => exprEqPrime(remainingTokens, rel, contextTokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprEqPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // (EQUAL | NOTEQ) exprRel exprEqPrime | eps
    tokens match
      case (op@(_: EqualToken | _: NoteqToken)) :: tail =>
        exprRel(tail, contextTokens) match
          case (Some(rel), IsParsed(remainingTokens)) =>
            exprEqPrime(remainingTokens, BinaryExprNode(prevExpr, op, rel), contextTokens)

          // should have expression after equality operators
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an equality expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprRel(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprAdd exprRelPrime
    exprAdd(tokens, contextTokens) match
      case (Some(add), IsParsed(remainingTokens)) => exprRelPrime(remainingTokens, add, contextTokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprRelPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrime | eps
    tokens match
      case (op@(_: LessToken | _: LesseqToken | _: GreaterToken | _: GreatereqToken)) :: tail =>
        exprAdd(tail, contextTokens) match
          case (Some(add), IsParsed(remainingTokens)) =>
            exprRelPrime(remainingTokens, BinaryExprNode(prevExpr, op, add), contextTokens)

          // should have expression after comparison operator
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of a comparison expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprAdd(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprMul exprAddPrime
    exprMul(tokens, contextTokens) match
      case (Some(mul), IsParsed(remainingTokens)) => exprAddPrime(remainingTokens, mul, contextTokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprAddPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // (ADD | SUB) exprMul exprAddPrime | eps
    tokens match
      case (op@(_: AddToken | _: SubToken)) :: tail =>
        exprMul(tail, contextTokens) match
          case (Some(mul), IsParsed(remainingTokens)) =>
            exprAddPrime(remainingTokens, BinaryExprNode(prevExpr, op, mul), contextTokens)

          // should have expression after operator
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an arithmetic expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprMul(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprCast exprMulPrime
    exprCast(tokens, contextTokens) match
      case (Some(cast), IsParsed(remainingTokens)) => exprMulPrime(remainingTokens, cast, contextTokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprMulPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // (MUL | DIV) exprCast exprMulPrime | eps
    tokens match
      case (op@(_: MulToken | _: DivToken)) :: tail =>
        exprCast(tail, contextTokens) match
          case (Some(cast), IsParsed(remainingTokens)) =>
            exprMulPrime(remainingTokens, BinaryExprNode(prevExpr, op, cast), contextTokens)

          // should have expression after operator
          case (None, NotParsed(t :: _)) =>
            throw SyntaxError("the right hand side of an arithmetic expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => (Some(prevExpr), IsParsed(tokens))


  def exprCast(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
    tokens match
      case LparToken(_) :: tail =>
        typeBase(tail) match
          case (Some(baseType), IsParsed(remainingTokens)) =>
            arraySize(remainingTokens, tokens) match
              case (arrDecl, remainingTokens) =>
                remainingTokens.get match
                  case RparToken(_) :: tail =>
                    exprCast(tail, contextTokens) match
                      case (Some(castExpr), remainingTokens) =>
                        (Some(CastExprNode(baseType, arrDecl, castExpr)), remainingTokens)
                      case _ => (None, NotParsed(tokens))

                  // should have the matching )
                  case t :: _ => throw SyntaxError(")", t, contextTokens)

                  case _ => (None, NotParsed(tokens))
          case _ => exprUnary(tokens, contextTokens)
      case _ => exprUnary(tokens, contextTokens)

  def exprUnary(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    tokens match
      case (op@(_: SubToken | _: NotToken)) :: tail =>
        exprUnary(tail, contextTokens) match
          case (Some(expUnary), remainingTokens) => (Some(UnaryExprNode(op, expUnary)), remainingTokens)

          // should have expression after operator
          case (None, NotParsed(t :: _)) => throw SyntaxError("an unary expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))
      case _ => exprPostfix(tokens, contextTokens)

  def exprPostfix(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // exprPrimary exprPostfixPrime
    exprPrimary(tokens, contextTokens) match
      case (Some(exp), IsParsed(remainingTokens)) =>
        exprPostfixPrime(remainingTokens, exp, contextTokens)
      case _ => (None, NotParsed(tokens))

  @tailrec
  private def exprPostfixPrime(tokens: Tokens, prevExpr: ExpressionNode, contextTokens: Tokens): ParsingPair[ExpressionNode] =
  // LBRACKET expr RBRACKET exprPostfixPrime | DOT ID exprPostfixPrime | eps
    tokens match
      case LbracketToken(_) :: tail =>
        expr(tail, contextTokens) match
          case (Some(exp), IsParsed(RbracketToken(_) :: tail)) =>
            exprPostfixPrime(tail, ArrayAccessExprNode(prevExpr, exp), contextTokens)

          // should have the matching ]
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError("]", t, contextTokens)

          // should have expression between []
          case (None, NotParsed(t :: _)) => throw SyntaxError("an expression", t, contextTokens)

          case _ => (None, NotParsed(tokens))

      case DotToken(_) :: (id@IdentifierToken(_, _)) :: tail =>
        exprPostfixPrime(tail, FieldAccessExprNode(prevExpr, id), contextTokens)

      // should have id after .
      case DotToken(_) :: t :: _ => throw SyntaxError("an identifier", t, contextTokens)

      case _ => (Some(prevExpr), IsParsed(tokens))

  def exprPrimary(tokens: Tokens, contextTokens: Tokens): ParsingPair[ExpressionNode] =
    // ID(LPAR(expr(COMMA expr) *) ? RPAR) ? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
    @tailrec
    def exprHelper(tokens: Tokens, expressions: List[ExpressionNode]): ParsingPair[List[ExpressionNode]] =
      tokens match
        case CommaToken(_) :: remainingTokens =>
          expr(remainingTokens, contextTokens) match
            case (Some(e), IsParsed(remainingTokens)) => exprHelper(remainingTokens, expressions :+ e)
            case _ => (Some(List()), IsParsed(tokens))

        // should have , between function arguments
        case t :: _ if !t.isInstanceOf[RparToken] && !t.isInstanceOf[LaccToken] => throw SyntaxError(",", t, contextTokens)

        case _ => (Some(expressions), IsParsed(tokens))

    tokens match
      // ID(LPAR(expr(COMMA expr) *) ? RPAR)
      case (funName@IdentifierToken(_, _)) :: LparToken(_) :: tail =>
        expr(tail, contextTokens) match
          case (Some(exp), IsParsed(remainingTokens)) =>
            exprHelper(remainingTokens, List(exp)) match
              case (Some(exprs), IsParsed(RparToken(_) :: remainingTokens)) =>
                (Option(FunctionCallExprNode(funName, exprs *)), IsParsed(remainingTokens))

              // should have ) at the end of function call
              case (Some(_), IsParsed(t :: _)) => throw SyntaxError(")", t, contextTokens)

              case _ => (None, NotParsed(tokens))

          // function called without params
          case (None, NotParsed(RparToken(_) :: remainingTokens)) => (Option(FunctionCallExprNode(funName)), IsParsed(remainingTokens))

          case _ => (None, NotParsed(tokens))

      // ID
      case (varName@IdentifierToken(_, _)) :: tail => (Some(VariableExprNode(varName)), IsParsed(tail))

      // INT | DOUBLE | CHAR | STRING
      case (t@_: LiteralToken[_]) :: tail => (Some(LiteralExprNode(t)), IsParsed(tail))

      // LPAR expr RPAR
      case LparToken(_) :: tail =>
        expr(tail, contextTokens) match
          case (exp, IsParsed(RparToken(_) :: tail)) => (exp, IsParsed(tail))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(")", t, contextTokens)

          case _ => (None, NotParsed(tokens))

      case _ => (None, NotParsed(tokens))
