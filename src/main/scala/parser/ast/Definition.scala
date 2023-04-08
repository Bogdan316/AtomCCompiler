package parser.ast

import parser.ast.Statement.{CompoundStm, compoundStm}
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import parser.utils.exceptions.ExpectedButFoundError
import token.Token
import token.TokenCode.*

import scala.annotation.tailrec

trait Definition extends Statement

object Definition:

  case class StructDef
  (
    id: Token,
    variableDefinitions: VariableDef*
  ) extends Definition

  case class VariableDef
  (
    typeBase: DefinitionUtils.TypeBase,
    id: Token,
    arraySize: Option[DefinitionUtils.ArraySize] = None
  ) extends Definition

  case class FunctionDef
  (
    returnType: DefinitionUtils.TypeBase,
    id: Token,
    compoundStm: CompoundStm,
    functionParams: DefinitionUtils.FunctionParam *
  ) extends Definition

  def variableDef(tokens: Tokens): ParsingPair[VariableDef] =
    // typeBase ID arrayDecl? SEMICOLON
    DefinitionUtils.typeBase(tokens) match
      case (Some(varType), IsParsed((varId@Token(ID, _)) :: remainingTokens)) =>

        DefinitionUtils.arraySize(remainingTokens, tokens) match
          case (arraySize, remainingTokens) =>
            remainingTokens.get match
              case Token(SEMICOLON, _) :: tail =>
                (Some(VariableDef(varType, varId, arraySize)), IsParsed(tail))

              // should have ; at the end of definition
              case t :: _ if t.tokenCode != LBRACKET =>
                throw ExpectedButFoundError(SEMICOLON, t, tokens)

              case _ => (None, NotParsed(tokens))

      // should have id after type
      case (Some(_), IsParsed(t :: _)) if t.tokenCode != LACC =>
        throw ExpectedButFoundError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))

  def structDef(tokens: Tokens): ParsingPair[StructDef] =
    // STRUCT ID LACC varDef* RACC SEMICOLON
    @tailrec
    def structDefHelper(crtTokens: Tokens, definitions: List[VariableDef]): ParsingPair[List[VariableDef]] =
      // varDef*
      variableDef(crtTokens) match
        case (Some(definition), IsParsed(remainingTokens)) => structDefHelper(remainingTokens, definitions :+ definition)
        case _ => (Option(definitions), IsParsed(crtTokens))

    tokens match
      case Token(STRUCT, _) :: (structId@Token(ID, _)) :: Token(LACC, _) :: remainingTokens =>

        structDefHelper(remainingTokens, List()) match
          case (Some(definitions), IsParsed(Token(RACC, _) :: Token(SEMICOLON, _) :: remainingTokens)) =>
            (Option(StructDef(structId, definitions *)), IsParsed(remainingTokens))

          // should have ; at end of struct definition
          case (Some(_), IsParsed(Token(RACC, _) :: t :: _)) =>
            throw ExpectedButFoundError(SEMICOLON, t, tokens)

          // should have matching }
          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(RACC, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have { after struct name
      case Token(STRUCT, _) :: Token(ID, _) :: t :: _ =>
        throw ExpectedButFoundError(LACC, t, tokens)

      // should have name after struct keyword
      case Token(STRUCT, _) :: t :: _ =>
        throw ExpectedButFoundError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))

  private def functionParams(tokens: Tokens, contextTokens: Tokens): ParsingPair[List[DefinitionUtils.FunctionParam]] =
    // (fnParam (COMMA fnParam)*)?
    @tailrec
    def functionParamsHelper(crtTokens: Tokens, params: List[DefinitionUtils.FunctionParam]): ParsingPair[List[DefinitionUtils.FunctionParam]] =
      // (COMMA fnParam)*
      crtTokens match
        case Token(COMMA, _) :: remainingTokens =>
          DefinitionUtils.functionParam(remainingTokens, contextTokens) match
            case (Some(param), IsParsed(remainingTokens)) => functionParamsHelper(remainingTokens, params :+ param)
            case _ => (Some(List()), IsParsed(crtTokens))

        // should have , between parameters
        case t :: _ if t.tokenCode != RPAR && t.tokenCode != LACC =>
          throw ExpectedButFoundError(COMMA, t, contextTokens)

        case _ => (Some(params), IsParsed(crtTokens))


    DefinitionUtils.functionParam(tokens, contextTokens) match
      case (Some(param), IsParsed(remainingTokens)) =>
        functionParamsHelper(remainingTokens, List()) match
          case (Some(params), remainingTokens@IsParsed(_)) => (Option(param +: params), remainingTokens)
          case _ => (Some(List(param)), IsParsed(remainingTokens))
      case _ => (Some(List()), IsParsed(tokens))

  def functionDef(tokens: Tokens): ParsingPair[FunctionDef] =
    // (typeBase | VOID) ID LPAR (fnParam(COMMA fnParam) *) ? RPAR stmCompound
    val (returnType, functionName, remainingTokens) =
      tokens match
        case (retType@Token(VOID, _)) :: (fnName@Token(ID, _)) :: remainingTokens =>
          (Some(DefinitionUtils.TypeBase(retType)), Some(fnName), IsParsed(remainingTokens))

        // should have name after type
        case Token(VOID, _) :: t :: _ =>
          throw ExpectedButFoundError(ID, t, tokens)

        case _ =>
          DefinitionUtils.typeBase(tokens) match
            case (Some(baseType), IsParsed((fnName@Token(ID, _)) :: remainingTokens)) =>
              (Some(baseType), Some(fnName), IsParsed(remainingTokens))

            // should have name after type
            case (Some(_), IsParsed(t :: _)) if t.tokenCode != LACC =>
              throw ExpectedButFoundError(ID, t, tokens)

            case _ => (None, None, NotParsed(tokens))

    remainingTokens match
      case IsParsed(Token(LPAR, _) :: remainingTokens) =>

        functionParams(remainingTokens, tokens) match
          case (Some(params), IsParsed(Token(RPAR, _) :: remainingTokens)) =>

            compoundStm(remainingTokens) match
              case (Some(statement), remainingTokens) =>
                (Option(FunctionDef(returnType.get, functionName.get, statement, params *)), remainingTokens)
              case _ => (None, NotParsed(tokens))

          // should have the ) after parameters list
          case (Some(_), IsParsed(t :: _)) =>
            throw ExpectedButFoundError(RPAR, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after function name
      case IsParsed(t :: Token(LPAR, _) :: _) =>
        throw ExpectedButFoundError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))
