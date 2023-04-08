package parser.parsed

import parser.parsed.Parsed
import token.Token

case class IsParsed[T <: List[Token]](tokens: T) extends Parsed(tokens)
