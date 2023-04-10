package parser.exceptions

import parser.parsed.Tokens
import token.Token.stringify
import token.{Token, TokenCode}

case class SyntaxError(message: String, found: Token, contextTokens: Tokens) extends
  RuntimeException(s"\n\nExpected '${message}' but found '${stringify(found)}' after:\n'" +
    s"${stringify(contextTokens.span(_ ne found)._1)}'\nat line ${found.line}.\n")

case object SyntaxError:
  def apply(expected: TokenCode, found: Token, contextTokens: Tokens): SyntaxError = 
    SyntaxError(stringify(expected), found, contextTokens)
  