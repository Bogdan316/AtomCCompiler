package token

import scala.annotation.tailrec

case class Token(tokenCode: TokenCode, line: Int):
  override def toString: String =
    s"$line\t$tokenCode"

object Token:
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

  def stringify(tokenCode: TokenCode): String =
    tokenCode match
      case TokenCode.TYPE_CHAR => "char"
      case TokenCode.TYPE_INT => "int"
      case TokenCode.TYPE_DOUBLE => "double"
      case TokenCode.STRUCT => "struct"
      case TokenCode.IF => "if"
      case TokenCode.ELSE => "else "
      case TokenCode.WHILE => "while"
      case TokenCode.VOID => "void"
      case TokenCode.RETURN => "return"

      case TokenCode.COMMA => ", "
      case TokenCode.SEMICOLON => ";"
      case TokenCode.LPAR => "("
      case TokenCode.RPAR => ")"
      case TokenCode.LBRACKET => "["
      case TokenCode.RBRACKET => "]"
      case TokenCode.LACC => "{"
      case TokenCode.RACC => "}"
      case TokenCode.END => "EOF"

      case TokenCode.ADD => " +"
      case TokenCode.SUB => " -"
      case TokenCode.MUL => " *"
      case TokenCode.DIV => " /"
      case TokenCode.DOT => "."
      case TokenCode.AND => " &&"
      case TokenCode.OR => " ||"
      case TokenCode.NOT => " ! "
      case TokenCode.ASSIGN => " ="
      case TokenCode.EQUAL => " =="
      case TokenCode.NOTEQ => " !="
      case TokenCode.LESS => " <"
      case TokenCode.LESSEQ => " <="
      case TokenCode.GREATER => " >"
      case TokenCode.GREATEREQ => " >="

      case TokenCode.ID => "an identifier"
      case TokenCode.INT => "an integer"
      case TokenCode.DOUBLE => "a double"
      case TokenCode.CHAR => "a char"
      case TokenCode.STRING => "a string"

  def stringify(token: Token): String =
    token.tokenCode match
      case TokenCode.ID | TokenCode.INT | TokenCode.DOUBLE | TokenCode.CHAR | TokenCode.STRING =>
        token match
          case TokenWithValue(_, _, value) => s"$value"
          case _ => ""

      case code => stringify(code)

  def stringify(tokens: List[Token]): String =
    if tokens.isEmpty then
      ""
    else
      tokens.tail.foldLeft((stringify(tokens.head), tokens.head, "\t"))((acc, token) => {
        val indentation = token.tokenCode match
          case TokenCode.LACC => acc._3 + "\t"
          case TokenCode.RACC => acc._3.substring(0, acc._3.length - 1)
          case _ => acc._3


        val stringTokens =
          if acc._2.line != token.line then
            acc._1 + "\n" + indentation + stringify(token).trim
          else
            acc._1 + stringify(token)

        (stringTokens, token, indentation)
      })._1.trim