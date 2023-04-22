package parser.ast


import parser.ast.AstNode.DefinitionNode.*
import parser.ast.AstNode.DefinitionUtils.*
import parser.ast.DefinitionUtilsRules.*
import parser.ast.StatementNodeRules.compoundStm
import parser.exceptions.SyntaxError
import parser.parsed.{IsParsed, NotParsed, ParsingPair, Tokens}
import token.Token.DelimiterToken.*
import token.Token.KeywordToken.*
import token.Token.LiteralToken.*
import token.Token.OperatorToken.*
import token.Token.TypeToken.*
import token.Token.{IdentifierToken, TokenWithValue}
import token.{Token, TokenUtils}

import scala.annotation.tailrec

object DefinitionNodeRules:

  def variableDef(tokens: Tokens): ParsingPair[VariableDefNode] =
  // typeBase ID arrayDecl? SEMICOLON
    typeBase(tokens) match
      case (Some(varType), IsParsed((varId@IdentifierToken(_, _)) :: remainingTokens)) =>

        arraySize(remainingTokens, tokens) match
          case (arraySize, remainingTokens) =>
            remainingTokens.get match
              case SemicolonToken(_) :: tail =>
                (Some(VariableDefNode(varType, varId, arraySize)), IsParsed(tail))

              // should have ; at the end of definition
              case (t: LbracketToken) :: _ => throw SyntaxError(";", t, tokens)

              case _ => (None, NotParsed(tokens))

      // should have id after type
      case (Some(_), IsParsed(t :: _)) if !t.isInstanceOf[LaccToken] => throw SyntaxError("an identifier", t, tokens)

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
      case StructTypeToken(_) :: (structId@IdentifierToken(_, _)) :: LaccToken(_) :: remainingTokens =>

        structDefHelper(remainingTokens, List()) match
          case (Some(definitions), IsParsed(RaccToken(_) :: SemicolonToken(_) :: remainingTokens)) =>
            (Option(StructDefNode(structId, definitions *)), IsParsed(remainingTokens))

          // should have ; at end of struct definition
          case (Some(_), IsParsed(RaccToken(_) :: t :: _)) => throw SyntaxError(";", t, tokens)

          // should have matching }
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError("}", t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have { after struct name
      case StructTypeToken(_) :: IdentifierToken(_, _) :: t :: _ => throw SyntaxError("{", t, tokens)

      // should have name after struct keyword
      case StructTypeToken(_) :: t :: _ => throw SyntaxError("an identifier", t, tokens)

      case _ => (None, NotParsed(tokens))

  private def functionParams(tokens: Tokens, contextTokens: Tokens): ParsingPair[List[FunctionParamNode]] =
    // (fnParam (COMMA fnParam)*)?
    @tailrec
    def functionParamsHelper(crtTokens: Tokens, params: List[FunctionParamNode]): ParsingPair[List[FunctionParamNode]] =
    // (COMMA fnParam)*
      crtTokens match
        case CommaToken(_) :: remainingTokens =>
          functionParam(remainingTokens, contextTokens) match
            case (Some(param), IsParsed(remainingTokens)) => functionParamsHelper(remainingTokens, params :+ param)
            case _ => (Some(List()), IsParsed(crtTokens))

        // should have , between parameters
        case t :: _ if !t.isInstanceOf[RparToken] && !t.isInstanceOf[LaccToken] => throw SyntaxError(",", t, contextTokens)

        case _ => (Some(params), IsParsed(crtTokens))


    functionParam(tokens, contextTokens) match
      case (Some(param), IsParsed(remainingTokens)) =>
        functionParamsHelper(remainingTokens, List()) match
          case (Some(params), remainingTokens@IsParsed(_)) => (Option(param +: params), remainingTokens)
          case _ => (Some(List(param)), IsParsed(remainingTokens))
      case _ => (Some(List()), IsParsed(tokens))

  def functionDef(tokens: Tokens): ParsingPair[FunctionDefNode] =
    // (typeBase | VOID) ID LPAR (fnParam(COMMA fnParam) *) ? RPAR stmCompound
    val (returnType, functionName, remainingTokens) =
      tokens match
        case (retType@VoidTypeToken(_)) :: (fnName@IdentifierToken(_, _)) :: remainingTokens =>
          (Some(TypeBaseNode(retType)), Some(fnName), IsParsed(remainingTokens))

        // should have name after type
        case VoidTypeToken(_) :: t :: _ => throw SyntaxError("an identifier", t, tokens)

        case _ =>
          typeBase(tokens) match
            case (Some(baseType), IsParsed((fnName@IdentifierToken(_, _)) :: remainingTokens)) =>
              (Some(baseType), Some(fnName), IsParsed(remainingTokens))

            // should have name after type
            case (Some(_), IsParsed(t :: _)) if !t.isInstanceOf[LaccToken] => throw SyntaxError("an identifier", t, tokens)

            case _ => (None, None, NotParsed(tokens))

    remainingTokens match
      case IsParsed(LparToken(_) :: remainingTokens) =>

        functionParams(remainingTokens, tokens) match
          case (Some(params), IsParsed(RparToken(_) :: remainingTokens)) =>

            compoundStm(remainingTokens) match
              case (Some(statement), remainingTokens) =>
                (Option(FunctionDefNode(returnType.get, functionName.get, statement, params *)), remainingTokens)
              case _ => (None, NotParsed(tokens))

          // should have the ) after parameters list
          case (Some(_), IsParsed(t :: _)) => throw SyntaxError(")", t, tokens)

          case _ => (None, NotParsed(tokens))

      // should have ( after function name
      case IsParsed(t :: LparToken(_) :: _) => throw SyntaxError("an identifier", t, tokens)

      case _ => (None, NotParsed(tokens))
