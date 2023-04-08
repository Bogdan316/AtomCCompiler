package parser.utils.exceptions

import parser.parsed.Tokens
import parser.utils.exceptions.ExpectedButFoundError
import token.Token.stringify
import token.{Token, TokenCode}

class ExpectedButFoundError(expected: TokenCode, found: Token, contextTokens: Tokens) extends
  SyntaxError(stringify(expected), found, contextTokens)
