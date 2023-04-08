package parser.utils

import parser.parsed.Tokens
import token.{Token, TokenCode}
import token.Token.stringify

class SyntaxError(message: String, line: Int) extends RuntimeException(s"${message}at line $line.\n\n")

class ExpectedButFoundError(expected: Either[Tokens, TokenCode], found: Token, context: Option[Tokens] = None) extends
  SyntaxError(s"\n\nExpected '${stringify(expected)}' but found '${stringify(found)}'${context.map(c => s" after:\n'${stringify(c)}'\n").getOrElse(" ")}", found.line)

object ExpectedButFoundError:

  def apply(expected: Token, found: Token, context: Option[Tokens]): ExpectedButFoundError = new ExpectedButFoundError(Left(List(expected)), found, context)

  def apply(expected: TokenCode, found: Token, context: Option[Tokens]): ExpectedButFoundError = new ExpectedButFoundError(Right(expected), found, context)

  def apply(expected: Tokens, found: Token, context: Option[Tokens]): ExpectedButFoundError = new ExpectedButFoundError(Left(expected), found, context)
