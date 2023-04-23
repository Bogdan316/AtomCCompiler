package parser.ast

import parser.ast.AstNode.StatementNode.*
import parser.ast.AstNode.{BodyStatementNode, StatementNode}
import parser.ast.DefinitionNodeRules.*
import parser.ast.ExpressionNodeRules.expr
import parser.exceptions.SyntaxError
import parser.parsed.Parsed.*
import parser.parsed.{ParsingPair, Tokens}
import token.Token
import token.Token.DelimiterToken.*
import token.Token.IdentifierToken
import token.Token.KeywordToken.{ElseToken, IfToken, ReturnToken, WhileToken}
import token.Token.OperatorToken.*

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
      case LaccToken(_) :: remainingTokens =>
        helper(remainingTokens, List()) match
          case (Some(stms), IsParsed(remainingTokens)) =>
            remainingTokens match
              case RaccToken(_) :: tail => (Option(CompoundStmNode(stms *)), IsParsed(tail))

              // should have matching }
              case t :: _ => throw SyntaxError("}", t, tokens)

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
      case IfToken(_) :: LparToken(_) :: tail =>
        expr(tail, tokens) match
          case (Some(ifExp), IsParsed(RparToken(_) :: tail)) =>
            statement(tail) match
              case (Some(thenBranch), IsParsed(ElseToken(_) :: tail)) =>
                statement(tail) match
                  case (elseBranch@Some(_), remainingTokens@IsParsed(_)) =>
                    (Some(IfStmNode(ifExp, thenBranch, elseBranch)), remainingTokens)
                  case _ => (None, NotParsed(tokens))

              case (Some(thenBranch), remainingTokens@IsParsed(_)) => (Some(IfStmNode(ifExp, thenBranch)), remainingTokens)

              case _ => (None, NotParsed(tokens))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(")", t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after if
      case IfToken(_) :: t :: _ => throw SyntaxError("(", t, tokens)

      // WHILE LPAR expr RPAR stm
      case WhileToken(_) :: LparToken(_) :: tail =>
        expr(tail, tokens) match
          case (Some(cond), IsParsed(RparToken(_) :: tail)) =>
            statement(tail) match
              case (Some(body), remainingTokens@IsParsed(_)) => (Some(WhileStmNode(cond, body)), remainingTokens)
              case _ => (None, NotParsed(tokens))

          // should have matching )
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(")", t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after while
      case WhileToken(_) :: t :: _ => throw SyntaxError("(", t, tokens)

      // RETURN expr? SEMICOLON
      case ReturnToken(_) :: tail =>
        expr(tail, tokens) match
          case (returnExpr, remainingTokens) =>
            remainingTokens.get match
              case SemicolonToken(_) :: tail => (Some(ReturnStmNode(returnExpr)), IsParsed(tail))

              // should have ; after expression
              case t :: _ => throw SyntaxError(";", t, tokens)

              case _ => (None, NotParsed(tokens))

      case LaccToken(_) :: _ => compoundStm(tokens)

      // expr? SEMICOLON
      case t =>
        expr(t, tokens) match
          case (expr@Some(_), IsParsed(SemicolonToken(_) :: tail)) => (Some(ExpressionStmNode(expr)), IsParsed(tail))

          case (None, NotParsed(SemicolonToken(_) :: tail)) => (Some(ExpressionStmNode()), IsParsed(tail))

          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(";", t, tokens)

          case _ => (None, NotParsed(tokens))
