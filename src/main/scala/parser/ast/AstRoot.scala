package parser.ast

import parser.ast.Definition.{functionDef, structDef, variableDef}
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import parser.utils.exceptions.ExpectedButFoundError
import token.Token
import token.TokenCode.{END, ID}

import scala.annotation.tailrec

trait AstRoot extends AstNode

object AstRoot:

  case class AstRoot(definitions: Definition*) extends AstNode

  def astRoot(tokens: Tokens): ParsingPair[AstRoot] =
    // (structDef | fnDef | varDef)* END
    @tailrec
    def helper(crtTokens: Tokens, definitions: List[Definition]): List[Definition] =
      functionDef(crtTokens) match
        case (Some(varDefinition), IsParsed(remainingTokens)) => helper(remainingTokens, definitions :+ varDefinition)
        case _ =>
          variableDef(crtTokens) match
            case (Some(structDefinition), IsParsed(remainingTokens)) =>
              helper(remainingTokens, definitions :+ structDefinition)
            case _ => structDef(crtTokens) match
              case (Some(functionDefinition), IsParsed(remainingTokens)) =>
                helper(remainingTokens, definitions :+ functionDefinition)
              case (_, remainingTokens) =>
                remainingTokens.get match
                  case Token(END, _) :: Nil => definitions
                  case t => throw ExpectedButFoundError(END, t.head, tokens)

    (Option(AstRoot(helper(tokens, List()) *)), IsParsed(tokens))