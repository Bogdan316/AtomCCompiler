package parser.parsed

import token.Token

type Tokens = List[Token]
type ParsingPair[T] = (Option[T], Parsed[Tokens])

sealed trait Parsed[T <: Tokens](tokens: T):
  def get: T = tokens

object Parsed:
  final case class IsParsed[T <: Tokens](tokens: T) extends Parsed(tokens)

  final case class NotParsed[T <: Tokens](tokens: T) extends Parsed(tokens)
  