package parser.parsed

import token.Token

trait Parsed[T <: List[Token]](tokens: T):
  def get: T = tokens
