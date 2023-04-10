package parser.parsed

import parser.parsed.Parsed
import token.Token

case class IsParsed[T <: Tokens](tokens: T) extends Parsed(tokens)
