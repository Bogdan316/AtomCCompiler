package token.helpers

import token.{Token, TokenUtils}
import token.Token.TypeToken.*
import token.Token.KeywordToken.*
import token.Token.LiteralToken.*
import token.Token.DelimiterToken.*
import token.Token.IdentifierToken
import token.Token.OperatorToken.*
import token.TokenUtils.{isChar, isString, isInteger, isDouble}

import scala.util.Try

object Tokenizers:
  def tokenizeKeyword(line: Int, token: String): Option[Token] =
    Option(
      token match
        case "char" => CharTypeToken(line)
        case "int" => IntTypeToken(line)
        case "double" => DoubleTypeToken(line)
        case "struct" => StructTypeToken(line)
        case "if" => IfToken(line)
        case "else" => ElseToken(line)
        case "while" => WhileToken(line)
        case "void" => VoidTypeToken(line)
        case "return" => ReturnToken(line)
        case _ => IdentifierToken(line, token)
    )

  def tokenizeDelimiter(line: Int, token: String): Option[Token] =
    Try(
      token match
        case "," => CommaToken(line)
        case ";" => SemicolonToken(line)
        case "(" => LparToken(line)
        case ")" => RparToken(line)
        case "[" => LbracketToken(line)
        case "]" => RbracketToken(line)
        case "{" => LaccToken(line)
        case "}" => RaccToken(line)
    ).toOption

  def tokenizeOperator(line: Int, token: String, nextChar: Char): Option[Token] =
    Try(
      token match
        case "+" => AddToken(line)
        case "-" => SubToken(line)
        case "*" => MulToken(line)
        case "/" => DivToken(line)
        case "." => DotToken(line)
        case "&&" => AndToken(line)
        case "||" => OrToken(line)
        case "==" => EqualToken(line)
        case "!=" => NoteqToken(line)
        case "<=" => LesseqToken(line)
        case ">=" => GreatereqToken(line)
        case "!" if nextChar != '=' => NotToken(line)
        case "=" if nextChar != '=' => AssignToken(line)
        case "<" if nextChar != '=' => LessToken(line)
        case ">" if nextChar != '=' => GreaterToken(line)
    ).toOption

  def tokenizeConstant(line: Int, token: String, nextChar: Char): Option[Token] =
    Try(token match
      case t if isChar(t) =>
        CharLiteralToken(line, t.replace("'", ""))

      case t if isString(t) =>
        StringLiteralToken(line, t.replace("\"", ""))

      case t if !".eE".contains(nextChar) && !nextChar.isDigit && isInteger(t) =>
        IntLiteralToken(line, t.toInt)

      case t if !nextChar.isDigit && isDouble(t) && !".eE+-".contains(nextChar) =>
        DoubleLiteralToken(line, t.toDouble)
    ).toOption
