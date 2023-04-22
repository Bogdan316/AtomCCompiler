package parser.ast

import parser.ast.AstNode.DefinitionUtils.*
import parser.exceptions.SyntaxError
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import token.TokenCode.{ID, INT, LBRACKET, RBRACKET, STRUCT, TYPE_CHAR, TYPE_DOUBLE, TYPE_INT}
import token.{Token, TokenWithValue}

object DefinitionUtilsRules:

  def typeBase(tokens: Tokens): ParsingPair[TypeBaseNode] =
    // TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
    tokens match
      case (definedType@Token(TYPE_INT | TYPE_DOUBLE | TYPE_CHAR, _)) :: tail =>
        (Option(TypeBaseNode(definedType)), IsParsed(tail))

      case (structType@Token(STRUCT, _)) :: (structId@Token(ID, _)) :: tail =>
        (Option(TypeBaseNode(structType, Option(structId))), IsParsed(tail))

      // struct keyword should be followed by an id
      case Token(STRUCT, _) :: t :: _ => throw SyntaxError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))

  def arraySize(tokens: Tokens, contextTokens: Tokens): ParsingPair[ArraySizeNode] =
    // LBRACKET INT? RBRACKET
    tokens match
      case Token(LBRACKET, _) :: Token(RBRACKET, _) :: tail => (Option(ArraySizeNode(Option(0))), IsParsed(tail))

      case Token(LBRACKET, _) :: TokenWithValue(INT, _, value: Int) :: Token(RBRACKET, _) :: tail =>
        (Option(ArraySizeNode(Option(value))), IsParsed(tail))

      // should have integer between []
      case Token(LBRACKET, _) :: t :: Token(RBRACKET, _) :: _ => throw SyntaxError(INT, t, contextTokens)

      // should have matching ]
      case Token(LBRACKET, _) :: t :: _ => throw SyntaxError(RBRACKET, t, contextTokens)

      case _ => (None, NotParsed(tokens))

  def functionParam(tokens: Tokens, contextTokens: Tokens): ParsingPair[FunctionParamNode] =
    // typeBase ID arrayDecl?
    typeBase(tokens) match
      case (Some(paramType), IsParsed((paramId@Token(ID, _)) :: remainingTokens)) =>
        arraySize(remainingTokens, contextTokens) match
          case (arraySize, remainingTokens) =>
            (Option(FunctionParamNode(paramType, paramId, arraySize)), IsParsed(remainingTokens.get))

      // should have name after type
      case (Some(_), IsParsed(t :: _)) => throw SyntaxError(ID, t, contextTokens)

      case _ => (None, NotParsed(tokens))
