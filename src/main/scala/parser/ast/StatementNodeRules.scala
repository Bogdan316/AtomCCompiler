package parser.ast

import parser.ast.AstNode.{BodyStatementNode, StatementNode}
import parser.ast.AstNode.StatementNode.*
import parser.ast.DefinitionNodeRules.*
import parser.ast.ExpressionNodeRules.expr
import parser.exceptions.SyntaxError
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import token.Token
import token.TokenCode.{ELSE, IF, LACC, LPAR, RACC, RETURN, RPAR, SEMICOLON, WHILE}

import scala.annotation.tailrec

object StatementNodeRules:

  def compoundStm(tokens: Tokens): ParsingPair[CompoundStmNode] =
    // LACC (varDef | stm)* RACC
    @tailrec
    def helper(crtTokens: Tokens, definitions: List[BodyStatementNode]): ParsingPair[List[BodyStatementNode]] =
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
              case Token(RACC, _) :: tail => (Option(CompoundStmNode(stms *)), IsParsed(tail))

              // should have matching }
              case t :: _ => throw SyntaxError(RACC, t, tokens)

              case _ => (None, NotParsed(tokens))
          case _ => (None, NotParsed(tokens))
      case _ => (None, NotParsed(tokens))

  def statement(tokens: Tokens): ParsingPair[StatementNode] =
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
                    (Some(IfStmNode(ifExp, thenBranch, elseBranch)), remainingTokens)
                  case _ => (None, NotParsed(tokens))

              case (Some(thenBranch), remainingTokens@IsParsed(_)) => (Some(IfStmNode(ifExp, thenBranch)), remainingTokens)

              case _ => (None, NotParsed(tokens))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(RPAR, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after if
      case Token(IF, _) :: t :: _ => throw SyntaxError(LPAR, t, tokens)

      // WHILE LPAR expr RPAR stm
      case Token(WHILE, _) :: Token(LPAR, _) :: tail =>
        expr(tail) match
          case (Some(cond), IsParsed(Token(RPAR, _) :: tail)) =>
            statement(tail) match
              case (Some(body), remainingTokens@IsParsed(_)) => (Some(WhileStmNode(cond, body)), remainingTokens)
              case _ => (None, NotParsed(tokens))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(RPAR, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after while
      case Token(WHILE, _) :: t :: _ => throw SyntaxError(LPAR, t, tokens)

      // RETURN expr? SEMICOLON
      case Token(RETURN, _) :: tail =>
        expr(tail) match
          case (returnExpr, remainingTokens) =>
            remainingTokens.get match
              case Token(SEMICOLON, _) :: tail => (Some(ReturnStmNode(returnExpr)), IsParsed(tail))

              // should have ; after expression
              case t :: _ => throw SyntaxError(SEMICOLON, t, tokens)

              case _ => (None, NotParsed(tokens))

      case Token(LACC, _) :: _ => compoundStm(tokens)

      // expr? SEMICOLON
      case t =>
        expr(t) match
          case (expr@Some(_), IsParsed(Token(SEMICOLON, _) :: tail)) => (Some(ExpressionStmNode(expr)), IsParsed(tail))

          case (None, NotParsed(Token(SEMICOLON, _) :: tail)) => (Some(ExpressionStmNode()), IsParsed(tail))

          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(SEMICOLON, t, tokens)

          case _ => (None, NotParsed(tokens))
