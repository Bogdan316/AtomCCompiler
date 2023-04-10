package parser.ast

import parser.ast.Statement.{CompoundStmNode, compoundStm}
import parser.exceptions.SyntaxError
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import token.Token
import token.TokenCode.*

import scala.annotation.tailrec

trait Definition extends Statement

object Definition:

  case class StructDefNode
  (
    id: Token,
    variableDefinitions: VariableDefNode*
  ) extends Definition

  case class VariableDefNode
  (
    typeBase: DefinitionUtils.TypeBaseNode,
    id: Token,
    arraySize: Option[DefinitionUtils.ArraySizeNode] = None
  ) extends Definition

  case class FunctionDefNode
  (
    returnType: DefinitionUtils.TypeBaseNode,
    id: Token,
    compoundStm: CompoundStmNode,
    functionParams: DefinitionUtils.FunctionParamNode *
  ) extends Definition

  def variableDef(tokens: Tokens): ParsingPair[VariableDefNode] =
    // typeBase ID arrayDecl? SEMICOLON
    DefinitionUtils.typeBase(tokens) match
      case (Some(varType), IsParsed((varId@Token(ID, _)) :: remainingTokens)) =>

        DefinitionUtils.arraySize(remainingTokens, tokens) match
          case (arraySize, remainingTokens) =>
            remainingTokens.get match
              case Token(SEMICOLON, _) :: tail =>
                (Some(VariableDefNode(varType, varId, arraySize)), IsParsed(tail))

              // should have ; at the end of definition
              case t :: _ if t.tokenCode != LBRACKET => throw SyntaxError(SEMICOLON, t, tokens)

              case _ => (None, NotParsed(tokens))

      // should have id after type
      case (Some(_), IsParsed(t :: _)) if t.tokenCode != LACC => throw SyntaxError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))

  def structDef(tokens: Tokens): ParsingPair[StructDefNode] =
    // STRUCT ID LACC varDef* RACC SEMICOLON
    @tailrec
    def structDefHelper(crtTokens: Tokens, definitions: List[VariableDefNode]): ParsingPair[List[VariableDefNode]] =
      // varDef*
      variableDef(crtTokens) match
        case (Some(definition), IsParsed(remainingTokens)) => structDefHelper(remainingTokens, definitions :+ definition)
        case _ => (Option(definitions), IsParsed(crtTokens))

    tokens match
      case Token(STRUCT, _) :: (structId@Token(ID, _)) :: Token(LACC, _) :: remainingTokens =>

        structDefHelper(remainingTokens, List()) match
          case (Some(definitions), IsParsed(Token(RACC, _) :: Token(SEMICOLON, _) :: remainingTokens)) =>
            (Option(StructDefNode(structId, definitions *)), IsParsed(remainingTokens))

          // should have ; at end of struct definition
          case (Some(_), IsParsed(Token(RACC, _) :: t :: _)) => throw SyntaxError(SEMICOLON, t, tokens)

          // should have matching }
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(RACC, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have { after struct name
      case Token(STRUCT, _) :: Token(ID, _) :: t :: _ => throw SyntaxError(LACC, t, tokens)

      // should have name after struct keyword
      case Token(STRUCT, _) :: t :: _ => throw SyntaxError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))

  private def functionParams(tokens: Tokens, contextTokens: Tokens): ParsingPair[List[DefinitionUtils.FunctionParamNode]] =
    // (fnParam (COMMA fnParam)*)?
    @tailrec
    def functionParamsHelper(crtTokens: Tokens, params: List[DefinitionUtils.FunctionParamNode]): ParsingPair[List[DefinitionUtils.FunctionParamNode]] =
      // (COMMA fnParam)*
      crtTokens match
        case Token(COMMA, _) :: remainingTokens =>
          DefinitionUtils.functionParam(remainingTokens, contextTokens) match
            case (Some(param), IsParsed(remainingTokens)) => functionParamsHelper(remainingTokens, params :+ param)
            case _ => (Some(List()), IsParsed(crtTokens))

        // should have , between parameters
        case t :: _ if t.tokenCode != RPAR && t.tokenCode != LACC => throw SyntaxError(COMMA, t, contextTokens)

        case _ => (Some(params), IsParsed(crtTokens))


    DefinitionUtils.functionParam(tokens, contextTokens) match
      case (Some(param), IsParsed(remainingTokens)) =>
        functionParamsHelper(remainingTokens, List()) match
          case (Some(params), remainingTokens@IsParsed(_)) => (Option(param +: params), remainingTokens)
          case _ => (Some(List(param)), IsParsed(remainingTokens))
      case _ => (Some(List()), IsParsed(tokens))

  def functionDef(tokens: Tokens): ParsingPair[FunctionDefNode] =
    // (typeBase | VOID) ID LPAR (fnParam(COMMA fnParam) *) ? RPAR stmCompound
    val (returnType, functionName, remainingTokens) =
      tokens match
        case (retType@Token(VOID, _)) :: (fnName@Token(ID, _)) :: remainingTokens =>
          (Some(DefinitionUtils.TypeBaseNode(retType)), Some(fnName), IsParsed(remainingTokens))

        // should have name after type
        case Token(VOID, _) :: t :: _ => throw SyntaxError(ID, t, tokens)

        case _ =>
          DefinitionUtils.typeBase(tokens) match
            case (Some(baseType), IsParsed((fnName@Token(ID, _)) :: remainingTokens)) =>
              (Some(baseType), Some(fnName), IsParsed(remainingTokens))

            // should have name after type
            case (Some(_), IsParsed(t :: _)) if t.tokenCode != LACC => throw SyntaxError(ID, t, tokens)

            case _ => (None, None, NotParsed(tokens))

    remainingTokens match
      case IsParsed(Token(LPAR, _) :: remainingTokens) =>

        functionParams(remainingTokens, tokens) match
          case (Some(params), IsParsed(Token(RPAR, _) :: remainingTokens)) =>

            compoundStm(remainingTokens) match
              case (Some(statement), remainingTokens) =>
                (Option(FunctionDefNode(returnType.get, functionName.get, statement, params *)), remainingTokens)
              case _ => (None, NotParsed(tokens))

          // should have the ) after parameters list
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(RPAR, t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after function name
      case IsParsed(t :: Token(LPAR, _) :: _) => throw SyntaxError(ID, t, tokens)

      case _ => (None, NotParsed(tokens))
