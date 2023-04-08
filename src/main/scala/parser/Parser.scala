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
    def unitHelper(crtTokens: Tokens, defs: List[Definition]): List[Definition] =
      functionDef(crtTokens) match
        case (Some(varDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ varDefinition)
        case _ =>
          variableDef(crtTokens) match
            case (Some(structDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ structDefinition)
            case _ => structDef(crtTokens) match
              case (Some(functionDefinition), IsParsed(remainingTokens)) => unitHelper(remainingTokens, defs :+ functionDefinition)
              case (None, NotParsed(Token(END, _) :: Nil)) => defs
              case (_, NotParsed(t :: _)) => throw ExpectedButFoundError(ID, t, Option(tokens.span(_ ne t)._1))
              case (_, IsParsed(t :: _)) => throw ExpectedButFoundError(ID, t, Option(tokens.span(_ ne t)._1))
              case _ => throw RuntimeException("Unexpected error.")

    (Option(UnitRule(unitHelper(tokens, List()) *)), IsParsed(tokens))

  def parse: AstNode =
    unit(originalTokens)._1.get

case object Parser extends App :
  val a = Lexer(new File("testlex.c")).tokenizeFile
  PrettyPrint.pprint(Parser(a).parse)
