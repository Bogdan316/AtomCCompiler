package parser.ast

import parser.ast.Definition.variableDef
import parser.ast.Expression.expr
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import parser.utils.exceptions.ExpectedButFoundError
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
    // LACC (varDef | stm)* RACC
    @tailrec
    def helper(crtTokens: Tokens, definitions: List[Statement]): ParsingPair[List[Statement]] =
      // (varDef | stm)*
      variableDef(crtTokens) match
        case (Some(definition), IsParsed(remainingTokens)) => helper(remainingTokens, definitions :+ definition)
        case _ =>
          statement(crtTokens) match
            case (Some(statement), IsParsed(remainingTokens)) => helper(remainingTokens, definitions :+ statement)
            case _ => (Some(definitions), IsParsed(crtTokens))

    tokens match
      case Token(LACC, _) :: remainingTokens =>
        helper(remainingTokens, List()) match
          case (Some(stms), IsParsed(remainingTokens)) =>
            remainingTokens match
              case Token(RACC, _) :: tail => (Option(CompoundStm(stms *)), IsParsed(tail))

              // should have matching }
              case t :: _ => throw ExpectedButFoundError(RACC, t, tokens)

              case _ => (None, NotParsed(tokens))
          case _ => (None, NotParsed(tokens))
      case _ => (None, NotParsed(tokens))

  def statement(tokens: Tokens): ParsingPair[Statement] =
    // stmCompound
    // | IF LPAR expr RPAR stm(ELSE stm) ?
    // | WHILE LPAR expr RPAR stm
    // | RETURN expr ? SEMICOLON
    // | expr ? SEMICOLON
    tokens match
      // IF LPAR expr RPAR stm(ELSE stm) ?
      case Token(IF, _) :: Token(LPAR, _) :: tail =>
        expr(tail) match
          case (Some(ifExp), IsParsed(Token(RPAR, _) :: tail)) =>
            statement(tail) match
              case (Some(thenBranch), IsParsed(Token(ELSE, _) :: tail)) =>
                statement(tail) match
                  case (elseBranch@Some(_), remainingTokens@IsParsed(_)) => 
                    (Some(IfStm(ifExp, thenBranch, elseBranch)), remainingTokens)
                  case _ => (None, NotParsed(tokens))

              case (Some(thenBranch), remainingTokens@IsParsed(_)) => (Some(IfStm(ifExp, thenBranch)), remainingTokens)

              case _ => (None, NotParsed(tokens))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw ExpectedButFoundError(RPAR, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after if
      case Token(IF, _) :: t :: _ => throw ExpectedButFoundError(LPAR, t, tokens)

      // WHILE LPAR expr RPAR stm
      case Token(WHILE, _) :: Token(LPAR, _) :: tail =>
        expr(tail) match
          case (Some(cond), IsParsed(Token(RPAR, _) :: tail)) =>
            statement(tail) match
              case (Some(body), remainingTokens@IsParsed(_)) => (Some(WhileStm(cond, body)), remainingTokens)
              case _ => (None, NotParsed(tokens))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw ExpectedButFoundError(RPAR, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after while
      case Token(WHILE, _) :: t :: _ => throw ExpectedButFoundError(LPAR, t, tokens)

      // RETURN expr? SEMICOLON
      case Token(RETURN, _) :: tail =>
        expr(tail) match
          case (returnExpr, remainingTokens) =>
            remainingTokens.get match
              case Token(SEMICOLON, _) :: tail => (Some(ReturnStm(returnExpr)), IsParsed(tail))

              // should have ; after expression
              case t :: _ => throw ExpectedButFoundError(SEMICOLON, t, tokens)

              case _ => (None, NotParsed(tokens))

      case Token(LACC, _) :: _ => compoundStm(tokens)

      // expr? SEMICOLON
      case t =>
        expr(t) match
          case (expr@Some(_), IsParsed(Token(SEMICOLON, _) :: tail)) => (Some(ExpressionStm(expr)), IsParsed(tail))

          case (None, NotParsed(Token(SEMICOLON, _) :: tail)) => (Some(ExpressionStm()), IsParsed(tail))

          case (Some(_), IsParsed(t :: _)) => throw ExpectedButFoundError(SEMICOLON, t, tokens)

          case _ => (None, NotParsed(tokens))
