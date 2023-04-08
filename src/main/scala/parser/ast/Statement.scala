package parser.ast

import parser.ast.Definition.variableDef
import parser.ast.Expression.expr
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import parser.utils.ExpectedButFoundError
import token.Token
import token.TokenCode.{ELSE, IF, LACC, LPAR, RACC, RETURN, RPAR, SEMICOLON, WHILE}

import scala.annotation.tailrec

trait Statement extends AstNode

object Statement:

  case class CompoundStm(statements: Statement*) extends Statement

  case class IfStm(condition: Expression, thenBranch: Statement, elseBranch: Option[Statement] = None) extends Statement

  case class WhileStm(condition: Expression, body: Statement) extends Statement

  case class ReturnStm(expr: Option[Expression] = None) extends Statement

  case class ExpressionStm(expr: Option[Expression] = None) extends Statement

  def compoundStm(tokens: Tokens): ParsingPair[CompoundStm] =
    @tailrec
    def compoundStmHelper(crtTokens: Tokens, definitions: List[Statement]): ParsingPair[List[Statement]] =
      variableDef(crtTokens) match
        case (Some(definition), IsParsed(remainingTokens)) => compoundStmHelper(remainingTokens, definitions :+ definition)
        case _ =>
          statement(crtTokens) match
            case (Some(statement), IsParsed(remainingTokens)) => compoundStmHelper(remainingTokens, definitions :+ statement)
            case _ => (Some(definitions), IsParsed(crtTokens))

    tokens match
      case Token(LACC, _) :: remainingTokens =>
        remainingTokens match
          case Token(RACC, _) :: tail => (Option(CompoundStm()), IsParsed(tail))
          case _ =>
            compoundStmHelper(remainingTokens, List()) match
              case (Some(stms), IsParsed(remainingTokens)) =>
                remainingTokens match
                  case Token(RACC, _) :: tail => (Option(CompoundStm(stms *)), IsParsed(tail))
                  case t :: _ => throw ExpectedButFoundError(RACC, t, Option(tokens.span(_ ne t)._1))
                  case _ => (None, NotParsed(tokens))
              case _ => (None, NotParsed(tokens))
      case _ => (None, NotParsed(tokens))

  def statement(tokens: Tokens): ParsingPair[Statement] =
    tokens match
      case Token(IF, _) :: Token(LPAR, _) :: tail =>
        expr(tail) match
          case (Some(ifExp), IsParsed(Token(RPAR, _) :: tail)) =>

            statement(tail) match
              case (Some(thenBranch), IsParsed(Token(ELSE, _) :: tail)) =>

                statement(tail) match
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

            statement(tail) match
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

      case Token(LACC, _) :: _ => compoundStm(tokens)

      case t =>
        expr(t) match
          case (expr@Some(_), IsParsed(Token(SEMICOLON, _) :: tail)) =>
            (Some(ExpressionStm(expr)), IsParsed(tail))

          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(SEMICOLON, t, Option(tokens.span(_ ne t)._1))

          case _ => (None, NotParsed(tokens))

