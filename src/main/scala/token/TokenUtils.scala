package token

import parser.parsed.Tokens
import token.{Token, TokenUtils}
import token.Token.TypeToken.*
import token.Token.KeywordToken.*
import token.Token.LiteralToken.*
import token.Token.DelimiterToken.*
import token.Token.IdentifierToken
import token.Token.OperatorToken.*
import token.Token.TokenWithValue

object TokenUtils:
  private val delimiters = ",;()[]{}"
  private val operators = "+-*/.&&||!==<=>="

  def isDelimiter(token: String): Boolean = delimiters.contains(token)

  def isOperator(token: String): Boolean = operators.contains(token)

  def isChar(token: String): Boolean = token.length == 3 && token.charAt(0) == '\'' && token.last == '\''

  def isString(token: String): Boolean = token.length >= 2 && token.charAt(0) == '\"' && token.last == '\"'

  def isInteger(token: String): Boolean = token.forall(_.isDigit) && token.nonEmpty

  private def isDoubleNoDot(token: String): Boolean =
    token.toLowerCase.split('e').toList match
      case base :: exponent :: Nil => isInteger(base) &&
        (isInteger(exponent) || ("+-".contains(exponent.charAt(0)) && isInteger(exponent.substring(1))))
      case _ => false

  private def isDoubleWithDot(token: String): Boolean =
    token.split('.').toList match
      case base :: exponent :: Nil => isInteger(base) && (isInteger(exponent) || isDoubleNoDot(exponent))
      case _ => false

  def isDouble(token: String): Boolean = isDoubleNoDot(token) || isDoubleWithDot(token)

  def isIdentifier(token: String): Boolean =
    (token.charAt(0).isLetter || token.charAt(0) == '_') && token.forall(isCharFromId)

  def isCharFromId(char: Char): Boolean =
    char.isLetterOrDigit || char == '_'

  def stringify(tokenCode: Token): String =
    tokenCode match
      case CharTypeToken(_) => "char"
      case IntTypeToken(_) => "int"
      case DoubleTypeToken(_) => "double"
      case StructTypeToken(_) => "struct"
      case VoidTypeToken(_) => "void"

      case IfToken(_) => "if"
      case ElseToken(_) => "else "
      case WhileToken(_) => "while"
      case ReturnToken(_) => "return"

      case CommaToken(_) => ","
      case SemicolonToken(_) => ";"
      case LparToken(_) => "("
      case RparToken(_) => ")"
      case LbracketToken(_) => "["
      case RbracketToken(_) => "]"
      case LaccToken(_) => "{"
      case RaccToken(_) => "}"
      case EndToken(_) => "EOF"

      case AddToken(_) => "+"
      case SubToken(_) => "-"
      case MulToken(_) => "*"
      case DivToken(_) => "/"
      case DotToken(_) => "."
      case AndToken(_) => "&&"
      case OrToken(_) => "||"
      case NotToken(_) => "! "
      case AssignToken(_) => "="
      case EqualToken(_) => "=="
      case NoteqToken(_) => "!="
      case LessToken(_) => "<"
      case LesseqToken(_) => "<="
      case GreaterToken(_) => ">"
      case GreatereqToken(_) => ">="

      case IdentifierToken(_, tokenValue) => tokenValue
      case IntLiteralToken(_, tokenValue) => tokenValue.toString
      case DoubleLiteralToken(_, tokenValue) => tokenValue.toString
      case CharLiteralToken(_, tokenValue) => tokenValue
      case StringLiteralToken(_, tokenValue) => tokenValue

  def stringify(tokens: Tokens): String =
    if tokens.isEmpty then
      ""
    else
      tokens.tail.foldLeft((stringify(tokens.head), tokens.head, "\t"))((acc, token) => {
        val indentation = token match
          case LaccToken(_) => acc._3 + "\t"
          case RaccToken(_) => acc._3.substring(0, acc._3.length - 1)
          case _ => acc._3


        val stringTokens =
          if acc._2.line != token.line then
            acc._1 + "\n" + indentation + stringify(token)
          else
            acc._1 + " " + stringify(token)

        (stringTokens, token, indentation)
      })._1.trim