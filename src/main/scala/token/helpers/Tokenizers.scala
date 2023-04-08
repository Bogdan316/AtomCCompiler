package token.helpers

import token.TokenCode.*
import token.{NoValueToken, Token, TokenWithValue}

import scala.util.Try

object Tokenizers:
  def tokenizeKeyword(line: Int, token: String): Option[Token] =
    Option(
      token match
        case "char" => NoValueToken(TYPE_CHAR, line)
        case "int" => NoValueToken(TYPE_INT, line)
        case "double" => NoValueToken(TYPE_DOUBLE, line)
        case "struct" => NoValueToken(STRUCT, line)
        case "if" => NoValueToken(IF, line)
        case "else" => NoValueToken(ELSE, line)
        case "while" => NoValueToken(WHILE, line)
        case "void" => NoValueToken(VOID, line)
        case "return" => NoValueToken(RETURN, line)
        case _ => new TokenWithValue[String](ID, line, token)
    )

  def tokenizeDelimiter(line: Int, token: String): Option[Token] =
    Try(
      token match
        case "," => NoValueToken(COMMA, line)
        case ";" => NoValueToken(SEMICOLON, line)
        case "(" => NoValueToken(LPAR, line)
        case ")" => NoValueToken(RPAR, line)
        case "[" => NoValueToken(LBRACKET, line)
        case "]" => NoValueToken(RBRACKET, line)
        case "{" => NoValueToken(LACC, line)
        case "}" => NoValueToken(RACC, line)
    ).toOption

  def tokenizeOperator(line: Int, token: String, nextChar: Char): Option[Token] =
    Try(
      token match
        case "+" => NoValueToken(ADD, line)
        case "-" => NoValueToken(SUB, line)
        case "*" => NoValueToken(MUL, line)
        case "/" => NoValueToken(DIV, line)
        case "." => NoValueToken(DOT, line)
        case "&&" => NoValueToken(AND, line)
        case "||" => NoValueToken(OR, line)
        case "==" => NoValueToken(EQUAL, line)
        case "!=" => NoValueToken(NOTEQ, line)
        case "<=" => NoValueToken(LESSEQ, line)
        case ">=" => NoValueToken(GREATEREQ, line)
        case "!" if nextChar != '=' => NoValueToken(NOT, line)
        case "=" if nextChar != '=' => NoValueToken(ASSIGN, line)
        case "<" if nextChar != '=' => NoValueToken(LESS, line)
        case ">" if nextChar != '=' => NoValueToken(GREATER, line)
    ).toOption

  def tokenizeConstant(line: Int, token: String, nextChar: Char): Option[Token] =
    Try(token match
      case t if Token.isChar(t) =>
        TokenWithValue[String](CHAR, line, t.replace("'", ""))

      case t if Token.isString(t) =>
        TokenWithValue[String](STRING, line, t.replace("\"", ""))

      case t if !".eE".contains(nextChar) && !nextChar.isDigit && Token.isInteger(t) =>
        TokenWithValue[Int](INT, line, t.toInt)

      case t if !nextChar.isDigit && Token.isDouble(t) && !".eE+-".contains(nextChar) =>
        TokenWithValue[Double](DOUBLE, line, t.toDouble)
    ).toOption
