package parser.exceptions

import parser.parsed.Tokens
import token.TokenUtils.stringify
import token.TokenUtils.stringify
import token.Token

case class SyntaxError(message: String, found: Token, contextTokens: Tokens) extends
  RuntimeException(s"\n\nExpected '${message}' but found '${stringify(found)}' after:\n'" +
    s"${stringify(contextTokens.span(_ ne found)._1)}'\nat line ${found.line}.\n")
