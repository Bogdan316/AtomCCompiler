package parser.ast

import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import parser.utils.exceptions.ExpectedButFoundError
import token.TokenCode.{ID, INT, LBRACKET, RBRACKET, STRUCT, TYPE_CHAR, TYPE_DOUBLE, TYPE_INT}
import token.{Token, TokenWithValue}

trait DefinitionUtils extends AstNode

object DefinitionUtils:

  case class TypeBase
  (
    baseType: Token,
    structId: Option[Token] = None
  ) extends DefinitionUtils

  case class ArraySize
  (
    size: Token = TokenWithValue(ID, -1, 0)
  ) extends DefinitionUtils

  case class FunctionParam
  (
    typeBase: TypeBase,
    id: Token,
    arraySize: Option[ArraySize] = None
  ) extends DefinitionUtils

  def typeBase(tokens: Tokens): ParsingPair[TypeBase] =
    // TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
    tokens match
      case (definedType@Token(TYPE_INT | TYPE_DOUBLE | TYPE_CHAR, _)) :: tail =>
        (Option(TypeBase(definedType)), IsParsed(tail))

      case (structType@Token(STRUCT, _)) :: (structId@Token(ID, _)) :: tail =>
        (Option(TypeBase(structType, Option(structId))), IsParsed(tail))

      // struct keyword should be followed by an id
      case Token(STRUCT, _) :: t :: _ =>
        throw ExpectedButFoundError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))

  def arraySize(tokens: Tokens, contextTokens: Tokens): ParsingPair[ArraySize] =
    // LBRACKET INT? RBRACKET
    tokens match
      case Token(LBRACKET, _) :: Token(RBRACKET, _) :: tail => (Option(ArraySize()), IsParsed(tail))

      case Token(LBRACKET, _) :: (arrSize@Token(INT, _)) :: Token(RBRACKET, _) :: tail =>
        (Option(ArraySize(arrSize)), IsParsed(tail))

      // should have integer between []
      case Token(LBRACKET, _) :: t :: Token(RBRACKET, _) :: _ =>
        throw ExpectedButFoundError(INT, t, contextTokens)

      // should have matching ]
      case Token(LBRACKET, _) :: t :: _ =>
        throw ExpectedButFoundError(RBRACKET, t, contextTokens)

      case _ => (None, NotParsed(tokens))

  def functionParam(tokens: Tokens, contextTokens: Tokens): ParsingPair[FunctionParam] =
    // typeBase ID arrayDecl?
    typeBase(tokens) match
      case (Some(paramType), IsParsed((paramId@Token(ID, _)) :: remainingTokens)) =>
        arraySize(remainingTokens, contextTokens) match
          case (arraySize, remainingTokens) =>
            (Option(FunctionParam(paramType, paramId, arraySize)), IsParsed(remainingTokens.get))

      // should have name after type
      case (Some(_), IsParsed(t :: _)) =>
        throw ExpectedButFoundError(ID, t, contextTokens)

      case _ => (None, NotParsed(tokens))
