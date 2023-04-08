package parser.ast

import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import parser.utils.ExpectedButFoundError
import token.TokenCode.{ID, INT, LBRACKET, RBRACKET, STRUCT, TYPE_CHAR, TYPE_DOUBLE, TYPE_INT}
import token.{Token, TokenWithValue}

trait Aux extends AstNode

object Aux:

  case class TypeBase(baseType: Token, structId: Option[Token] = None) extends Aux

  case class ArraySize(size: Token = TokenWithValue(ID, -1, 0)) extends Aux

  case class FunctionParam(typeBase: TypeBase, id: Token, arraySize: Option[ArraySize] = None) extends Aux

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
