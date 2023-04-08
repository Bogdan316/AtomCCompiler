package parser.utils.exceptions

import parser.parsed.Tokens
import parser.utils.exceptions.SyntaxError
import token.Token.stringify
import token.{Token, TokenCode}

class SyntaxError(message: String, found: Token, contextTokens: Tokens) extends
  RuntimeException(s"\n\nExpected '${message}' but found '${stringify(found)}' after:\n'" +
    s"${stringify(contextTokens.span(_ ne found)._1)}'\nat line ${found.line}.\n")
