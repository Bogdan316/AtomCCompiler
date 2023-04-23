package parser.ast

import parser.ast.AstNode.DefinitionNode.*
import parser.ast.AstNode.DefinitionUtils.*
import parser.ast.DefinitionUtilsRules.*
import parser.ast.StatementNodeRules.compoundStm
import parser.exceptions.SyntaxError
import parser.parsed.{ParsingPair, Tokens}
import parser.parsed.Parsed.*
import token.Token.DelimiterToken.*
import token.Token.KeywordToken.*
import token.Token.LiteralToken.*
import token.Token.OperatorToken.*
import token.Token.TypeToken.*
import token.Token.{IdentifierToken, TokenWithValue}
import token.{Token, TokenUtils}

object DefinitionUtilsRules:

  def typeBase(tokens: Tokens): ParsingPair[TypeBaseNode] =
  // TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
    tokens match
      case (definedType@(_: IntTypeToken | _: DoubleTypeToken | _: CharTypeToken)) :: tail =>
        (Option(TypeBaseNode(definedType)), IsParsed(tail))

      case (structType@StructTypeToken(_)) :: (structId@IdentifierToken(_, _)) :: tail =>
        (Option(TypeBaseNode(structType, Option(structId))), IsParsed(tail))

      // struct keyword should be followed by an id
      case StructTypeToken(_) :: t :: _ => throw SyntaxError("an identifier", t, tokens)

      case _ => (None, NotParsed(tokens))

  def arraySize(tokens: Tokens, contextTokens: Tokens): ParsingPair[ArraySizeNode] =
  // LBRACKET INT? RBRACKET
    tokens match
      case LbracketToken(_) :: RbracketToken(_) :: tail => (Option(ArraySizeNode(Option(0))), IsParsed(tail))

      case LbracketToken(_) :: IntLiteralToken(_, value) :: RbracketToken(_) :: tail =>
        (Option(ArraySizeNode(Option(value))), IsParsed(tail))

      // should have integer between []
      case LbracketToken(_) :: t :: RbracketToken(_) :: _ => throw SyntaxError("an integer", t, contextTokens)

      // should have matching ]
      case LbracketToken(_) :: t :: _ => throw SyntaxError("]", t, contextTokens)

      case _ => (None, NotParsed(tokens))

  def functionParam(tokens: Tokens, contextTokens: Tokens): ParsingPair[FunctionParamNode] =
  // typeBase ID arrayDecl?
    typeBase(tokens) match
      case (Some(paramType), IsParsed((paramId@IdentifierToken(_, _)) :: remainingTokens)) =>
        arraySize(remainingTokens, contextTokens) match
          case (arraySize, remainingTokens) =>
            (Option(FunctionParamNode(paramType, paramId, arraySize)), IsParsed(remainingTokens.get))

      // should have name after type
      case (Some(_), IsParsed(t :: _)) => throw SyntaxError("an identifier", t, contextTokens)

      case _ => (None, NotParsed(tokens))
