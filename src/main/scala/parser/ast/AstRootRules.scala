package parser.ast

import parser.ast.AstNode.{AstRoot, DefinitionNode}
import parser.ast.DefinitionNodeRules.*
import parser.exceptions.SyntaxError
import parser.parsed.{ParsingPair, Tokens}
import parser.parsed.Parsed.*
import token.Token
import token.Token.DelimiterToken.EndToken

import scala.annotation.tailrec


object AstRootRules:
  def astRoot(tokens: Tokens): ParsingPair[AstRoot] =
    // (structDef | fnDef | varDef)* END
    @tailrec
    def helper(crtTokens: Tokens, definitions: List[DefinitionNode]): List[DefinitionNode] =
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
                  case EndToken(_) :: Nil => definitions
                  case t => throw SyntaxError("EOF", t.head, tokens)

    (Option(AstRoot(helper(tokens, List()) *)), IsParsed(tokens))
