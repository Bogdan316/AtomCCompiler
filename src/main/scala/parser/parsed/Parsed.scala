package parser.parsed

import token.Token

type Tokens = List[Token]
type ParsingPair[T] = (Option[T], Parsed[Tokens])

trait Parsed[T <: Tokens](tokens: T):
  def get: T = tokens
